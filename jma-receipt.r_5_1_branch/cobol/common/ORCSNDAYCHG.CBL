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
       PROGRAM-ID.                 ORCSNDAYCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療日一括入力変換処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/11/20    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     太田         2018/3/23 入力領域を拡張(54->100)
      *                                     初期値は54で据え置く
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-KAI             PIC 9(01).
           03  FLG-HI              PIC 9(01).
           03  FLG-ALLSUJI         PIC 9(01).
           03  FLG-AST             PIC 9(01).
      *
           03  FLG-ZENKAKU         PIC 9(01).
           03  FLG-HANKAKU         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(02).
           03  IDX-D1              PIC 9(02).
           03  IDX-D2              PIC 9(02).
           03  IDX-Z               PIC 9(02).
      *
           03  IDK                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KOUMOKU         PIC X(50).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-STR             PIC 9(02).
           03  WRK-END             PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-LST-DD              PIC 9(02).
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
      *
      *    03  WRK-HENKAN-MAE          PIC X(50).
      *    03  WRK-HENKAN-ATO          PIC X(50).
      *
       01  WRK-NYUMOJI-AREA.
           03  WRK-HI              PIC 9(02).
           03  WRK-NISUU           PIC X(100).
           03  WRK-MAE-CD          PIC X(01).
           03  WRK-ACCT-MCNT       PIC 9(02).
           03  WRK-KAI             PIC X(100).
           03  WRK-MCNT1           PIC 9(02).
      *
      *    入院期間
       01  TBL-NYUIN-KAISU-AREA.
           03  TBL-NYUIN-OCC           OCCURS   31.
               05  TBL-HI              PIC 9(02).
               05  TBL-SEL             PIC X(01).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角・半角チェック
           COPY    "CPORCXKANACONV.INC".
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ
           COPY    "CPORCSNDAYCHG.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSNDAYCHGAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主　処理
           IF      ORCSNDAYCHG-ERRCD   =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *    エラーメッセージ編集
           IF     (ORCSNDAYCHG-KBN         =   "1" )  AND
                  (ORCSNDAYCHG-ERRCD   NOT =   SPACE )
               PERFORM 300-ERRMSG-HEN-SEC
           END-IF
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC             SECTION.
      *
           INITIALIZE                      WRK-NYUMOJI-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      ORCSNDAYCHG-OUTAREA
      *    最大文字数
           IF      ORCSNDAYCHG-KETA    =   ZERO
               MOVE    54                  TO  ORCSNDAYCHG-KETA
           END-IF
      *    最大文字数チェック
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    ORCSNDAYCHG-INPUTCD TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1        >   ORCSNDAYCHG-KETA
               MOVE    "0001"              TO  ORCSNDAYCHG-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC             SECTION.
      *
      *    入院回数変換
           PERFORM 2001-NYUINKAISU-SEC
      *
           IF     (ORCSNDAYCHG-KBN         =   "1" )  AND
                  (ORCSNDAYCHG-ERRCD       =   SPACE )
      *        入院期間日付チェック
               PERFORM 2002-DAY-CHEK-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入院の回数の変換処理
      *****************************************************************
       2001-NYUINKAISU-SEC             SECTION.
      *
           MOVE    ORCSNDAYCHG-SRYYMD  TO  WRK-SRYYMD
      *
           INITIALIZE                      WRK-NYUMOJI-AREA
           MOVE    SPACE               TO  WRK-KAI
           MOVE    SPACE               TO  WRK-NISUU
           MOVE    ZERO                TO  WRK-ACCT-MCNT
           MOVE    ZERO                TO  WRK-MCNT1
           MOVE    ZERO                TO  FLG-AST
           MOVE    ZERO                TO  FLG-KAI
           MOVE    ZERO                TO  FLG-HI
           MOVE    ZERO                TO  IDX-D1
           MOVE    ZERO                TO  IDX-D2
           INITIALIZE                  TBL-NYUIN-KAISU-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   ORCSNDAYCHG-KETA
      *
               EVALUATE    ORCSNDAYCHG-INPUTCD(IDX:1)
                   WHEN    "*"
                       MOVE    1               TO  FLG-AST
                   WHEN    "/"
                       ADD     1               TO  FLG-KAI
                   WHEN    "."
                   WHEN    ","
                   WHEN    "-"
      *                数字チェック
                       MOVE    ORCSNDAYCHG-INPUTCD(IDX:1)
                                               TO  WRK-MAE-CD
                       PERFORM 2001-NISUUSNUM-CHK-SEC
                       MOVE    SPACE           TO  WRK-NISUU
                       MOVE    ZERO            TO  WRK-MCNT1
                       MOVE    1               TO  FLG-HI
                   WHEN    OTHER
      ***              DISPLAY "WRK-ACCT-MCNT=" WRK-ACCT-MCNT
      *                回数
                       IF      (FLG-AST        =   1   )  AND
                               (FLG-KAI        =   ZERO)
                           ADD     1               TO  WRK-ACCT-MCNT
                           MOVE    ORCSNDAYCHG-INPUTCD (IDX:1)
                                                   TO  WRK-KAI
                                                      (WRK-ACCT-MCNT:1)
                       END-IF
      *                日数・回数の数値チェック
      ***              DISPLAY "WRK-MCNT1=" WRK-MCNT1
                       IF      (FLG-AST        =   1   )  AND
                               (FLG-KAI        =   1   )
                           ADD     1               TO  WRK-MCNT1
                           MOVE    ORCSNDAYCHG-INPUTCD (IDX:1)
                                                   TO  WRK-NISUU
                                                      (WRK-MCNT1:1)
                       END-IF
                   END-EVALUATE
           END-PERFORM
      *
           IF     (FLG-KAI             =   ZERO ) AND
                  (FLG-HI              =   1    )
               MOVE    "9009"              TO  ORCSNDAYCHG-ERRCD
           END-IF
           IF      FLG-KAI             >   1
               MOVE    "9010"              TO  ORCSNDAYCHG-ERRCD
           END-IF
      *
      *    数字チェック
           IF      WRK-NISUU       NOT =   SPACE
                MOVE    SPACE              TO  WRK-MAE-CD
                PERFORM 2001-NISUUSNUM-CHK-SEC
           END-IF
      *
      *    回数
           MOVE    WRK-ACCT-MCNT       TO  ORCSNDAYCHG-KAISUCNT
      *    回数　数字変換
           MOVE    ZERO                TO  ORCSNDAYCHG-KAISU
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-KAI             TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   3     )   OR
                  (SNUM-SYOSUU         >   ZERO  )
               MOVE    "0012"              TO  ORCSNDAYCHG-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  ORCSNDAYCHG-KAISU
           END-IF
      *
      *    期間編集
           INITIALIZE                      ORCSNDAYCHG-NYUIN-KAISU
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D2      FROM    1   BY  1
                   UNTIL      (IDX-D2      >   31       )
                       OR     (ORCSNDAYCHG-ERRCD   NOT =   SPACE )
                       OR     (TBL-HI(IDX-D2)  =   ZERO )
               IF     (IDX-D2                  >   1   )  AND
                      (TBL-SEL (IDX-D2 - 1)    =   "-" )  AND
                      (TBL-HI  (IDX-D2 - 1)    <   TBL-HI (IDX-D2))
                   COMPUTE IDX-Z           =   TBL-HI (IDX-D2 - 1)
                                           +   1
                   PERFORM VARYING     IDX2    FROM    IDX-Z  BY  1
                           UNTIL       IDX2    >=  TBL-HI (IDX-D2)
                       MOVE    IDX2            TO  IDX-ZAI
                       MOVE    IDX-ZAI         TO  ORCSNDAYCHG-NYU-DAY
                                                           (IDX-ZAI)
                   END-PERFORM
               END-IF
               MOVE    TBL-HI (IDX-D2)     TO  IDX-ZAI
               MOVE    TBL-HI (IDX-D2)     TO  ORCSNDAYCHG-NYU-DAY
                                                          (IDX-ZAI)
           END-PERFORM
      *
           .
       2001-NYUINKAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数字チェック処理
      *****************************************************************
       2001-NISUUSNUM-CHK-SEC    SECTION.
      *
      *    回数　数字変換
           IF      WRK-NISUU           =   SPACE
               MOVE    ZERO                TO  WRK-HI
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-NISUU           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   2     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    "9012"              TO  ORCSNDAYCHG-ERRCD
                   MOVE    ZERO                TO  WRK-HI
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-HI
               END-IF
           END-IF
      *
           IF      ORCSNDAYCHG-ERRCD   =   SPACE
               IF      WRK-HI              >   31
                   MOVE    "9006"          TO  ORCSNDAYCHG-ERRCD
               END-IF
           END-IF
      *
           IF      ORCSNDAYCHG-ERRCD   =   SPACE
               EVALUATE    WRK-MAE-CD
               WHEN    SPACE
               WHEN    "."
               WHEN    ","
                   IF      WRK-HI          =   ZERO
                      MOVE    WRK-SRYDD        TO  WRK-HI
                   END-IF
                   IF      IDX-D1              >   ZERO
                       IF     (TBL-SEL(IDX-D1) =   "-"  ) AND
                              (WRK-HI          <   TBL-HI (IDX-D1))
                           MOVE    "9013"          TO  ORCSNDAYCHG-ERRCD
                       END-IF
                   END-IF
                   ADD     1                   TO  IDX-D1
                   MOVE    WRK-HI              TO  TBL-HI (IDX-D1)
               WHEN    "-"
                   IF      WRK-HI          =   ZERO
                      MOVE    WRK-SRYDD        TO  WRK-HI
                   END-IF
                   ADD     1                   TO  IDX-D1
                   MOVE    WRK-HI              TO  TBL-HI (IDX-D1)
                   MOVE    "-"                 TO  TBL-SEL(IDX-D1)
               END-EVALUATE
           END-IF
      *
           .
       2001-NISUUSNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院期間日付チェック処理
      *****************************************************************
       2002-DAY-CHEK-SEC    SECTION.
      *
      *    診療日の末日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    ORCSNDAYCHG-SRYYMD(1:6)
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)
                                           TO  WRK-LST-DD
           ELSE
               MOVE    31                  TO  WRK-LST-DD
           END-IF
      *
           IF      ORCSNDAYCHG-NYUINYMD(1:6)
                                           =   ORCSNDAYCHG-SRYYMD(1:6)
               MOVE    ORCSNDAYCHG-NYUINYMD(7:2)   TO  WRK-STDD
           ELSE
               MOVE    01                          TO  WRK-STDD
           END-IF
           IF      ORCSNDAYCHG-TAIINYMD(1:6)
                                           =   ORCSNDAYCHG-SRYYMD(1:6)
               MOVE    ORCSNDAYCHG-TAIINYMD(7:2)   TO  WRK-EDDD
           ELSE
               MOVE    WRK-LST-DD                  TO  WRK-EDDD
           END-IF
           PERFORM VARYING     IDX2        FROM    1   BY  1
                   UNTIL      (IDX2        >   31  )
                          OR  (ORCSNDAYCHG-ERRCD   NOT =   SPACE )
               IF      ORCSNDAYCHG-NYU-DAY (IDX2)  NOT =   ZERO
                   IF     (IDX2                >=  WRK-STDD )  AND
                          (IDX2                <=  WRK-EDDD )
                       CONTINUE
                   ELSE
                       MOVE    "0002"          TO  ORCSNDAYCHG-ERRCD
                  END-IF
               END-IF
           END-PERFORM
      *
           .
       2002-DAY-CHEK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       300-ERRMSG-HEN-SEC               SECTION.
      *
           EVALUATE    ORCSNDAYCHG-ERRCD
               WHEN    "0001"
                   MOVE    "文字数オーバー"
                                               TO  ORCSNDAYCHG-ERRMSG
               WHEN    "0002"
                   MOVE    "入院中の日ではありません。"
                                               TO  ORCSNDAYCHG-ERRMSG
               WHEN    "0012"
                   MOVE    "数量・回数等の数字・桁数エラーです"
                                               TO  ORCSNDAYCHG-ERRMSG
               WHEN    "9006"
                   MOVE    "日付は３１までです。"
                                               TO  ORCSNDAYCHG-ERRMSG
               WHEN    "9009"
                   MOVE    "入院回数の／を入力して下さい。"
                                               TO  ORCSNDAYCHG-ERRMSG
               WHEN    "9010"
                   MOVE    "入院回数の／は１回のみです。"
                                               TO  ORCSNDAYCHG-ERRMSG
               WHEN    "9012"
                   MOVE    "日付は２桁までです。"
                                               TO  ORCSNDAYCHG-ERRMSG
               WHEN    "9013"
                   MOVE    "日付−日付の期間が違います。"
                                               TO  ORCSNDAYCHG-ERRMSG
           END-EVALUATE
      *
           .
       300-ERRMSG-HEN-EXT.
           EXIT.
      *
