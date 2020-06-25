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
       PROGRAM-ID.                 ORCSAPIHKNALL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通（ＡＰＩ）
      *  コンポーネント名  : 全保険組合せ編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/04/25    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-LABOR-SIO       PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
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
           03  WRK-START-YM            PIC X(06).
           03  WRK-END-YM              PIC X(06).
      *
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-HZN-RATE            PIC 9(03).
      *
      *    保険組み合わせ
       01  TBL-HKNCOMBI-AREA.
           02  TBL-HKNCOMBI-REC        OCCURS   200.
           COPY    "CPHKNCOMBI.INC"    REPLACING
                                       //COMB-// BY //TBL-COMB-//.
      *
      *    患者保険情報
       01  TBL-PTHKNINF-AREA.
         02  TBL-PTHKNINF-REC           OCCURS  100.
           COPY    "CPPTHKNINF.INC"     REPLACING
                                        //PTHKN-// BY //TBL-PTHKN-//.
      *
      *    公費マスタ（テーブル）
       01  TBL-PTKOHINF-AREA.
         02  TBL-PTKOHINF-REC           OCCURS  200.
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
      *    COPY  "CPSK1008.INC".
      *
           COPY  "CPSK1010.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK4002.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    労働基準監督署マスタ
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
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
      ***  COPY    "CPORCSKANACHK.INC".
      *    年齢・負担割合サブ
           COPY    "CPORCSAGECHK.INC".
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
           COPY    "CPORCSAPIHKNALL.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSAPIHKNALLAREA
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
      *    初期処理
           PERFORM 100-INIT-SYORI-SEC
      *
      *    保険組合せ編集処理
           PERFORM 200-MAIN-SYORI-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SYORI-SEC            SECTION.
      *
      *    開始・終了日設定
           MOVE    SPACE               TO  WRK-START-YM
           MOVE    SPACE               TO  WRK-END-YM
           IF      ORCSAPIHKN-START-YM =   SPACE
               IF      ORCSAPIHKN-END-YM   NOT =   SPACE
                   MOVE    ALL "0"             TO  WRK-START-YM
               END-IF
           ELSE
               MOVE    ORCSAPIHKN-START-YM     TO  WRK-START-YM
           END-IF
           IF      ORCSAPIHKN-END-YM   =   SPACE
               IF      ORCSAPIHKN-START-YM NOT =   SPACE
                   MOVE    ALL "9"             TO  WRK-END-YM
               END-IF
           ELSE
               MOVE    ORCSAPIHKN-END-YM     TO  WRK-END-YM
           END-IF
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
                               ( IDH-MAX       >=  100 )
               IF     (PTHKN-SAKJOKBN      =   SPACE )
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
                               ( IDK-MAX       >=  200 )
               IF     (PTKOH-SAKJOKBN      =   SPACE )
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
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200-MAIN-SYORI-SEC            SECTION.
      *
      *    保険組合マスター編集
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
           MOVE    ZERO                TO  IDXH
           MOVE    ZERO                TO  FLG-ERR
           PERFORM UNTIL   (FLG-HKNCOMBI   =   1   )
                       OR  (IDX-MAX        >   200 )
               MOVE    ZERO                TO  FLG-CHK-OK
               IF     (COMB-DELKBN         =   SPACE     ) AND
                      (COMB-HYOJIKBN   NOT =   "1"       )
                   MOVE    1                   TO  FLG-CHK-OK
      *            指定日のみ対象
                   IF     (ORCSAPIHKN-KBN          =   "1"   )
                     AND  (ORCSAPIHKN-SYSYMD   NOT =   SPACE )
                       IF     (COMB-TEKSTYMD   <=  ORCSAPIHKN-SYSYMD )
                         AND  (COMB-TEKEDYMD   >=  ORCSAPIHKN-SYSYMD )
                           MOVE    1               TO  FLG-CHK-OK
                       ELSE
                           MOVE    ZERO            TO  FLG-CHK-OK
                       END-IF
                   END-IF
      *            開始年月あり
                   IF     (WRK-START-YM        =   SPACE )
                      AND (WRK-END-YM          =   SPACE )
                       CONTINUE
                   ELSE
                       IF     (COMB-TEKEDYMD(1:6)  <   WRK-START-YM)
                          OR  (COMB-TEKSTYMD(1:6)  >   WRK-END-YM  )
      *                    対象外
                           MOVE    ZERO            TO  FLG-CHK-OK
                       END-IF
                   END-IF
               END-IF
               IF      FLG-CHK-OK            =   1
                   ADD     1                 TO  IDX-MAX
                   IF      IDX-MAX           >   200
                       MOVE    1                 TO  FLG-ERR
                   ELSE
      *                保険組合せ編集処理
                       PERFORM 2001-HKNCOMBI-HENSYU-SEC
                   END-IF
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
      *    対象２００件以上
           IF      FLG-ERR             =   1
               MOVE    1                   TO  ORCSAPIHKN-ERRCD
           END-IF
      *    対象なし
           IF      IDXH                =   ZERO
               MOVE    2                   TO  ORCSAPIHKN-ERRCD
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       2001-HKNCOMBI-HENSYU-SEC             SECTION.
      *
           ADD     1                   TO  IDXH
           MOVE    IDXH                TO  ORCSAPIHKN-OT-MAX
      *
           MOVE    COMB-HKNCOMBINUM
                                       TO  ORCSAPIHKN-THKNCOMBI
                                                               (IDXH)
           MOVE    COMB-HKNNUM
                                       TO  ORCSAPIHKN-HKNNUM (IDXH)
      *    保険ＩＤ
           MOVE    COMB-HKNID
                                       TO  ORCSAPIHKN-NAI-HKNID (IDXH)
      *    主保険−短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  ORCSAPIHKN-HKNNUM-NAME
                                                                 (IDXH)
      *    本人家族区分
           MOVE    COMB-HONKZKKBN
                                       TO  ORCSAPIHKN-HONKZKKBN(IDXH)
      *    補助区分
           MOVE    COMB-HOJOKBN
                                       TO  ORCSAPIHKN-HOJOKBN (IDXH)
           PERFORM 20011-HOJOKBN-NAME-HEN-SEC
      *
      *    継続区分
           MOVE    COMB-CONTKBN        TO  ORCSAPIHKN-CONTKBN (IDXH)
      *    負担割合
           IF     (ORCSAPIHKN-KBN          =   "1"   )
              AND (ORCSAPIHKN-SYSYMD   NOT =   SPACE )
      *        処理日付での負担割合
               PERFORM 20016-AGEFTNRATE-SEC
           ELSE
      *        保険組合せ負担割合
               MOVE    COMB-GAI-KFTNRATE   TO  WRK-HZN-RATE
               PERFORM 4001-FTNRATE-HEN-SEC
               MOVE    WRK-FTNRATEMEI
                                       TO  ORCSAPIHKN-COMB-RATEG(IDXH)
               MOVE    COMB-NYU-KFTNRATE   TO  WRK-HZN-RATE
               PERFORM 4001-FTNRATE-HEN-SEC
               MOVE    WRK-FTNRATEMEI
                                       TO  ORCSAPIHKN-COMB-RATEN(IDXH)
           END-IF
      *    有効期間
           MOVE    COMB-TEKSTYMD       TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE       TO  ORCSAPIHKN-COMB-TEKSTYMD(IDXH)
           MOVE    COMB-TEKEDYMD      TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE       TO  ORCSAPIHKN-COMB-TEKEDYMD(IDXH)
      *
           MOVE    COMB-TEKSTYMD      TO  ORCSAPIHKN-COMBN-TEKSTYMD
                                                                (IDXH)
           MOVE    COMB-TEKEDYMD      TO  ORCSAPIHKN-COMBN-TEKEDYMD
                                                                (IDXH)
      *    非表示区分
           MOVE    COMB-HYOJIKBN      TO  ORCSAPIHKN-HYOJIKBN (IDXH)
      *
      *    保険情報
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH         >   IDH-MAX  )
               IF      COMB-HKNID          =   TBL-PTHKN-HKNID (IDH)
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
      *    公費情報
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ     >   4    )
                         OR   (COMB-KOHID (IDZ)    =   ZERO )
               PERFORM 20012-PTKOHINF-HEN-SEC
           END-PERFORM
      *
      *    労災・自賠責・第三者行為編集
           IF     (COMB-HKNNUM         =   "971"  OR  "973" )
              OR  (COMB-KOHHKNNUM (1)  =   "970"  )
               PERFORM 20013-PTRSIINF-HEN-SEC
           END-IF
           .
       2001-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助区分名称編集
      *****************************************************************
       20011-HOJOKBN-NAME-HEN-SEC     SECTION.
      *
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    COMB-HKNNUM         TO  API012SUB1-HKNNUM
           MOVE    COMB-HOJOKBN        TO  API012SUB1-HOJOKBN
           MOVE    SPA-SYSYMD          TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
           MOVE    API012SUB1-HOJOKBN-NAME
                                       TO  ORCSAPIHKN-HOJOKBN-NAME(IDXH)
      *
           .
       20011-HOJOKBN-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       20012-PTKOHINF-HEN-SEC           SECTION.
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   IDK-MAX )
      *        公費情報
               IF      COMB-KOHID (IDZ)    =   TBL-PTKOH-KOHID (IDH)
      *            公費ＩＤ
                   MOVE    TBL-PTKOH-KOHID (IDH)
                                           TO  ORCSAPIHKN-NAI-KOHID
                                                            (IDXH IDZ)
      *            公費の種類
                   MOVE    TBL-PTKOH-KOHNUM (IDH)
                                           TO  ORCSAPIHKN-KOHNUM
                                                            (IDXH IDZ)
      *            公費の名称
                   MOVE    COMB-KOH-TANSEIDONAME (IDZ)
                                           TO  ORCSAPIHKN-KOHNUM-NAME
                                                            (IDXH IDZ)
      *            負担者番号
                   MOVE    TBL-PTKOH-FTNJANUM (IDH)
                                           TO  ORCSAPIHKN-FTNJANUM
                                                            (IDXH IDZ)
      *            受給者番号
                   MOVE    TBL-PTKOH-JKYSNUM (IDH)
                                           TO  ORCSAPIHKN-JKYSNUM
                                                            (IDXH IDZ)
      *            有効開始日
                   MOVE    TBL-PTKOH-TEKSTYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKSTYMD
                                                            (IDXH IDZ)
      *            有効終了日
                   MOVE    TBL-PTKOH-TEKEDYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKEDYMD
                                                            (IDXH IDZ)
      *            確認日付
                   IF      TBL-PTKOH-KAKUNINYMD (IDH)  NOT =   SPACE
                       MOVE    TBL-PTKOH-KAKUNINYMD (IDH)  TO  WRK-SYMD
                       PERFORM 800-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  ORCSAPIHKN-KOH-KAKUNINYMD
                                                            (IDXH IDZ)
                   END-IF
      *
      *            負担率・負担額編集
                   PERFORM 400-KOHFTN-HENSYU-SEC
      *
                   MOVE    IDK-MAX             TO  IDH
               END-IF
           END-PERFORM
           .
       20012-PTKOHINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    労災・自賠責・第三者行為編集処理
      *****************************************************************
       20013-PTRSIINF-HEN-SEC                SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           IF      COMB-HKNNUM         =   "971"  OR  "973"
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
           ELSE
               MOVE    COMB-KOHID (1)      TO  PTRSI-KOHID
           END-IF
      *
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
           IF      FLG-PTRSIINF        =   1
               GO      TO    20013-PTRSIINF-HEN-EXT
           END-IF
      *    労災保険名称
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "短期給付"      TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "2"
                   MOVE    "傷病年金"      TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "3"
                   MOVE    "アフターケア"  TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "4"
                   MOVE    "自賠責保険"    TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "5"
                   MOVE    "公務災害"      TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
               WHEN    "6"
                   MOVE    "第三者行為"    TO  ORCSAPIHKN-ACCIDENT-NAME
                                                                  (IDXH)
           END-EVALUATE
      *    傷病の部位
           MOVE    PTRSI-COMMENT       TO  ORCSAPIHKN-DISEASE-LOCATION
                                                                 (IDXH)
      *    傷病年月日
           IF      PTRSI-SHOBYOYMD NOT =   SPACE
               MOVE    PTRSI-SHOBYOYMD     TO  WRK-SYMD
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-DISEASE-DATE(IDXH)
           END-IF
      *
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
      *    労働保険番号
                   MOVE    PTRSI-KOFUNUM
                                       TO  ORCSAPIHKN-ACCIDENT-NUMBER
                                                                (IDXH)
               WHEN    "2"
      *    年金証書番号
                   MOVE    PTRSI-KOFUNUM
                                       TO  ORCSAPIHKN-PENSIONCERTIFICATE
                                                                (IDXH)
               WHEN    "3"
      *    健康管理手帳番号
                   MOVE    PTRSI-KOFUNUM
                                   TO  ORCSAPIHKN-RSI-KANRITCHO-NUMBER
                                                                (IDXH)
           END-EVALUATE
      *    災害区分
           MOVE    PTRSI-SAIGAIKBN     TO  ORCSAPIHKN-ACCIDENT-CLASS
                                                               (IDXH)
      *    災害区分（名称）
           EVALUATE    PTRSI-SAIGAIKBN
               WHEN    "1"
                   MOVE    "業務中の災害"
                                   TO  ORCSAPIHKN-ACCIDENT-CLASS-NAME
                                                               (IDXH)
               WHEN    "2"
                   MOVE    "通勤途上の災害"
                                   TO  ORCSAPIHKN-ACCIDENT-CLASS-NAME
                                                               (IDXH)
           END-EVALUATE
      *
      *    新継再別区分
           MOVE    PTRSI-SINKEI    TO  ORCSAPIHKN-SINKEI (IDXH)
           EVALUATE    PTRSI-SINKEI
               WHEN    "1"
                   MOVE    "初診"      TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
               WHEN    "3"
                   MOVE    "転医始診"  TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
               WHEN    "5"
                   MOVE    "継続"      TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
               WHEN    "7"
                   MOVE    "再発"      TO  ORCSAPIHKN-SINKEI-NAME(IDXH)
           END-EVALUATE
      *    転帰事由
           MOVE    PTRSI-TENKI         TO  ORCSAPIHKN-TENKI (IDXH)
           EVALUATE    PTRSI-TENKI
               WHEN    "1"
                   MOVE    "治癒"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "3"
                   MOVE    "継続"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "5"
                   MOVE    "転医"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "7"
                   MOVE    "中止"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
               WHEN    "9"
                   MOVE    "死亡"      TO  ORCSAPIHKN-TENKI-NAME(IDXH)
           END-EVALUATE
      *    四肢特例区分
           MOVE    PTRSI-SISIKBN       TO  ORCSAPIHKN-SISIKBN (IDXH)
           EVALUATE    PTRSI-SISIKBN
               WHEN    "0"
                   MOVE    "なし"      TO  ORCSAPIHKN-SISIKBN-NAME(IDXH)
               WHEN    "1"
                   MOVE    "四肢"      TO  ORCSAPIHKN-SISIKBN-NAME(IDXH)
               WHEN    "2"
                   MOVE    "手指"      TO  ORCSAPIHKN-SISIKBN-NAME(IDXH)
           END-EVALUATE
      *    労働基準監督署コード
           MOVE    PTRSI-SYOCD         TO  ORCSAPIHKN-LABOR-ST-CODE
                                                               (IDXH)
           IF      PTRSI-SYOCD     NOT =   SPACE
               PERFORM 200131-SYOCD-HEN-SEC
           END-IF
      *
      *    事業所名称
           MOVE    PTRSI-JIGYOUNAME    TO  ORCSAPIHKN-RSI-JIGYOUNAME
                                                                (IDXH)
      *    所在地都道府県名
           MOVE    PTRSI-PREFNAME      TO  ORCSAPIHKN-RSI-PREFNAME
                                                               (IDXH)
      *    所在地都道府県区分
           MOVE    PTRSI-PREFKBN       TO  ORCSAPIHKN-RSI-PREFKBN(IDXH)
           EVALUATE    PTRSI-PREFKBN
               WHEN    "1"
                   MOVE    "都"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
               WHEN    "2"
                   MOVE    "道"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
               WHEN    "3"
                   MOVE    "府"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
               WHEN    "4"
                   MOVE    "県"        TO  ORCSAPIHKN-RSI-PREFKBNNAME
                                                               (IDXH)
           END-EVALUATE
      *    所在地郡市区名
           MOVE    PTRSI-CITYNAME      TO  ORCSAPIHKN-RSI-CITYNAME(IDXH)
      *    所在地郡市区区分
           MOVE    PTRSI-CITYKBN       TO  ORCSAPIHKN-RSI-CITYKBN(IDXH)
           EVALUATE    PTRSI-CITYKBN
               WHEN    "1"
                   MOVE    "郡"        TO  ORCSAPIHKN-RSI-CITYKBNNAME
                                                               (IDXH)
               WHEN    "2"
                   MOVE    "市"        TO  ORCSAPIHKN-RSI-CITYKBNNAME
                                                               (IDXH)
               WHEN    "3"
                   MOVE    "区"        TO  ORCSAPIHKN-RSI-CITYKBNNAME
                                                               (IDXH)
           END-EVALUATE
      *    労災レセ回数記載 基準年月
           IF      PTRSI-KIJYUNYM  NOT =   SPACE
               MOVE    PTRSI-KIJYUNYM      TO  WRK-SYMD(1:6)
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE(1:7)   TO  ORCSAPIHKN-KIJYUNYM
                                                               (IDXH)
           END-IF
      *    労災レセ回数記載 回数
           IF      PTRSI-KAISU     NOT =   ZERO
               MOVE    PTRSI-KAISU     TO  ORCSAPIHKN-RESEPUTO-COUNT
                                                               (IDXH)
           END-IF
      *自賠責
      *    自賠責保険会社名
           MOVE    PTRSI-HOKENNAME     TO  ORCSAPIHKN-RSI-HOKENNAME
                                                               (IDXH)
      *    自賠責患者請求区分
           MOVE    PTRSI-JIBAISEIKBN   TO  ORCSAPIHKN-JIBAISEIKBN(IDXH)
           EVALUATE    PTRSI-JIBAISEIKBN
               WHEN    "1"
                   MOVE    "患者請求する(100%)"
                                       TO  ORCSAPIHKN-JIBAISEIKBN-NAME
                                                            (IDXH)
               WHEN    "2"
                   MOVE    "患者請求しない(0%)"
                                       TO  ORCSAPIHKN-JIBAISEIKBN-NAME
                                                            (IDXH)
           END-EVALUATE
      *    自賠責担当医コード
           IF      PTRSI-DRCD      NOT =   SPACE
               MOVE    PTRSI-DRCD      TO  ORCSAPIHKN-RSI-DRCD (IDXH)
      *
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "1010"              TO  SYS-KANRICD
               MOVE    PTRSI-RYOSTYMD      TO  SYS-STYUKYMD
               MOVE    PTRSI-RYOSTYMD      TO  SYS-EDYUKYMD
               MOVE    PTRSI-DRCD          TO  SYS-KBNCD
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME   TO  ORCSAPIHKN-RSI-DRCD-NAME
                                                                (IDXH)
               END-IF
           END-IF
      *
      *アフターケア
           EVALUATE    PTRSI-HKNKBN
           WHEN    "3"
      *        損傷区分
               STRING  PTRSI-SONSHOUKBN
                       PTRSI-SONSHOUKBN-EDANUM
                                           DELIMITED  BY  SIZE
                                       INTO   ORCSAPIHKN-DAMAGE-CLASS
                                                                (IDXH)
               END-STRING
      *        損傷区分名称取得
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "4002"              TO  SYS-KANRICD
      *        アフター療養開始年月日
               MOVE    PTRSI-SINSATUYMD    TO  SYS-STYUKYMD
               MOVE    PTRSI-SINSATUYMD    TO  SYS-EDYUKYMD
               MOVE    ORCSAPIHKN-DAMAGE-CLASS (IDXH)
                                           TO  SYS-KBNCD
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-4002-REC
                   MOVE    SYS-4002-SONSHOUNAME
                                       TO  ORCSAPIHKN-DAMAGE-CLASS-NAME
                                                                (IDXH)
               END-IF
      *    第三者行為
           WHEN    "6"
      *        現物支給区分
               MOVE    PTRSI-DAISAN-GENBTKBN
                                       TO  ORCSAPIHKN-DS-GENBTKBN(IDXH)
               EVALUATE    PTRSI-DAISAN-GENBTKBN
               WHEN    "1"
                   MOVE    "対象外"
                                   TO  ORCSAPIHKN-DS-GENBTKBN-NAME(IDXH)
               WHEN    "2"
                   MOVE    "対象"
                                   TO  ORCSAPIHKN-DS-GENBTKBN-NAME(IDXH)
               END-EVALUATE
      *        特記事項区分
               MOVE    PTRSI-DAISAN-TOKKIKBN
                                       TO  ORCSAPIHKN-DS-TOKKIKBN(IDXH)
               EVALUATE    PTRSI-DAISAN-TOKKIKBN
               WHEN    "1"
                   MOVE    "「１０第三」記載有"
                                   TO  ORCSAPIHKN-DS-TOKKIKBN-NAME(IDXH)
               WHEN    "2"
                   MOVE    "「１０第三」記載無"
                                   TO  ORCSAPIHKN-DS-TOKKIKBN-NAME(IDXH)
               END-EVALUATE
           END-EVALUATE
           .
       20013-PTRSIINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労働基準監督署名称編集処理
      *****************************************************************
       200131-SYOCD-HEN-SEC                SECTION.
      *
           INITIALIZE                      LABOR-SIO-REC
           MOVE    PTRSI-SYOCD         TO  LABOR-SIO-SYOCD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOSTYMD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOEDYMD
      *
           MOVE    LABOR-SIO-REC       TO  MCPDATA-REC
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_labor_sio"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  LABOR-SIO-REC
                   MOVE    ZERO                TO  FLG-LABOR-SIO
               ELSE
                   MOVE    1                   TO  FLG-LABOR-SIO
                   INITIALIZE                  LABOR-SIO-REC
               END-IF
           ELSE
               INITIALIZE                  LABOR-SIO-REC
               MOVE    1               TO  FLG-LABOR-SIO
           END-IF
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-LABOR-SIO       =   ZERO
               MOVE    LABOR-SIO-NAME      TO  ORCSAPIHKN-LABOR-ST-NAME
                                                             (IDXH)
           END-IF
           .
       200131-SYOCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ負担割合編集処理
      *****************************************************************
       20016-AGEFTNRATE-SEC            SECTION.
      *
      *    入院
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    ORCSAPIHKN-SYSYMD   TO  AGECHK-SRYYMD
           MOVE    ORCSAPIHKN-BIRTHDAY TO  AGECHK-BIRTHDAY
           MOVE    "1"                 TO  AGECHK-NYUGAIKBN
           MOVE    COMB-HKNCOMBINUM
                                       TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-FTNRATE      TO  WRK-HZN-RATE
      *    負担割合計算
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-COMB-RATEN(IDXH)
      *
      *    外来
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    ORCSAPIHKN-SYSYMD   TO  AGECHK-SRYYMD
           MOVE    ORCSAPIHKN-BIRTHDAY TO  AGECHK-BIRTHDAY
           MOVE    "2"                 TO  AGECHK-NYUGAIKBN
           MOVE    COMB-HKNCOMBINUM
                                       TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-FTNRATE      TO  WRK-HZN-RATE
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-COMB-RATEG(IDXH)
      *
           .
       20016-AGEFTNRATE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    負担率編集処理
      *****************************************************************
       4001-FTNRATE-HEN-SEC        SECTION.
      *
           COMPUTE WRK-FTNRATE         =   WRK-HZN-RATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
      *
           .
       4001-FTNRATE-HEN-EXT.
           EXIT.
      *
      *
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
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
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
           MOVE    HKN-HON-GAIKFTNRATE TO  WRK-HZN-RATE
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-RATE-OUTPATIENT
                                                            (IDXH IDZ)
      *    外来上限額
           IF      HKN-HON-GAIKKOTGAK  =   ZERO
      *        本人−外来−日−上限額
               MOVE    HKN-HON-GAIDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        本人−外来−回−上限額
               MOVE    HKN-HON-GAIKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  ORCSAPIHKN-MONEY-OUTPATIENT
                                                            (IDXH IDZ)
      *
      *    本人−入院−回−負担割合
           MOVE    HKN-HON-NYUKFTNRATE TO  WRK-HZN-RATE
           PERFORM 4001-FTNRATE-HEN-SEC
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-RATE-ADMISSION
                                                            (IDXH IDZ)
      *    外来上限額
           IF      HKN-HON-NYUKKOTGAK  =   ZERO
      *        本人−入院−日−上限額
               MOVE    HKN-HON-NYUDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        本人−入院−回−上限額
               MOVE    HKN-HON-NYUKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  ORCSAPIHKN-MONEY-ADMISSION
                                                            (IDXH IDZ)
           .
       400-KOHFTN-HENSYU-EXT.
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
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEYREAD-SEC        SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
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
