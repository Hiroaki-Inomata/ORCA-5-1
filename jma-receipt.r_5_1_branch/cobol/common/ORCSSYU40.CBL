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
       PROGRAM-ID.                 ORCSSYU40.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 福岡県地方公費対応
      *                      給付対象外点数編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/25    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  04/03/11  Ｈ１６．４改正対応
      *  01.00.02    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  01.00.03    NACL-多々納  06/03/20  Ｈ１８．４改正対応
      *  01.00.04    NACL-多々納  06/04/12  電子化加算追加
      *  01.00.05    NACL-多々納  06/08/18  同日初診追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.08.00    NACL-多々納  12/03/XX  Ｈ２５年４月改正対応
      *  04.08.00    NACL-多々納  12/03/XX  Ｈ２７年１月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYOSIN-ERR      PIC 9(01).
           03  FLG-SYOSIN          PIC 9(01).
           03  FLG-DOUJITU         PIC 9(01).
      *
           03  FLG-SYORI-END       PIC 9(01).
           03  FLG-SYONIKA         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-D2              PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KASAN           PIC X(01).
      *    診療行為計算用
           03  WRK-TENSU           PIC 9(08).
           03  WRK-ZAITEN          PIC 9(08).
      *
           03  WRK-SRYCD           PIC X(09).
           03  WRK-HEN-SRYCD       PIC X(09).
      *    初診算定日
           03  WRK-DD              PIC 9(02).
           03  WRK-DD2             PIC 9(02).
           03  WRK-DD-S            PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *    電子化加算
           03  WRK-DENSIKA         PIC 9(01).
      *    同日初診
           03  WRK-DOUJITU         PIC X(01).
      *
      *    診療行為コード一覧
       01  TBL-SINSATU-AREA.
      *    初診料（一般 病院）
           03  FILLER              PIC X(13)   VALUE   "111000110 1 0".
      *    初診料（老人 病院）
           03  FILLER              PIC X(13)   VALUE   "111700110 1 0".
      *    初診料（一般 診療所）
           03  FILLER              PIC X(13)   VALUE   "111003610 1 0".
      *    初診料（老人 診療所）
           03  FILLER              PIC X(13)   VALUE   "111700210 1 0".
      *
      *    往診料
           03  FILLER              PIC X(13)   VALUE   "114000110 1 0".
      *    往診（夜間加算）
           03  FILLER              PIC X(13)   VALUE   "114000470 2 1".
      *    往診（深夜加算）
           03  FILLER              PIC X(13)   VALUE   "114000570 2 2".
      *    往診（緊急加算）
           03  FILLER              PIC X(13)   VALUE   "114000370 2 2".
      *
      *    特別往診料
           03  FILLER              PIC X(13)   VALUE   "114001610 1 0".
      *    特別往診（夜間加算）
           03  FILLER              PIC X(13)   VALUE   "114001970 2 1".
      *    特別往診（深夜加算）
           03  FILLER              PIC X(13)   VALUE   "114002070 2 2".
      *    特別往診（緊急加算）
           03  FILLER              PIC X(13)   VALUE   "114001870 2 2".
      *
      *
      *    小児科外来診察料(院外処方 初診)
           03  FILLER              PIC X(13)   VALUE   "113003510 1 0".
      *    小児科外来診察料(院内処方 初診)
           03  FILLER              PIC X(13)   VALUE   "113003710 1 0".
      *    小児科外来診察料(初診時時間外加算)
           03  FILLER              PIC X(13)   VALUE   "111010470 2 1".
      *    小児科外来診察料(初診時休日加算)
           03  FILLER              PIC X(13)   VALUE   "111010570 2 2".
      *    小児科外来診察料(初診時深夜加算)
           03  FILLER              PIC X(13)   VALUE   "111010670 2 3".
      *    小児科外来診察料(初診時時間外特例)
           03  FILLER              PIC X(13)   VALUE   "111010770 2 4".
      *
      *    小児科外来診察料(初診時乳幼児夜間）
           03  FILLER              PIC X(13)   VALUE   "113007070 2 5".
      *    小児科外来診察料(初診時乳幼児休日）
           03  FILLER              PIC X(13)   VALUE   "113007170 2 6".
      *    小児科外来診察料(初診時乳幼児深夜）
           03  FILLER              PIC X(13)   VALUE   "113007270 2 7".
      *Ｈ１８／４から
      *    小児科外来診察料(初診時時間外加算)
           03  FILLER              PIC X(13)   VALUE   "113009670 2 1".
      *
      *    初診料（同日複数診療科）
           03  FILLER              PIC X(13)   VALUE   "111011810 110".
      *H25.4 から
      *    初診料（紹介状なし）
           03  FILLER              PIC X(13)   VALUE   "111012510 1 0".
      *    同日初診（紹介状なし）
           03  FILLER              PIC X(13)   VALUE   "111012610 110".
      *H27.1 から
      *    初診料（妥結率５割以下）
           03  FILLER              PIC X(13)   VALUE   "111012710 1 0".
      *H29.2
      *    初診料（同一日２科目・妥結率５割以下）
           03  FILLER              PIC X(13)   VALUE   "111012810 110".
      *
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
      *****03  TBL-SINSATU-OCC         OCCURS  18    INDEXED  TBL-IDX.
           03  TBL-SINSATU-OCC         OCCURS  27    INDEXED  TBL-IDX.
      *        診療コード
               05  TBL-SRYCD           PIC X(09).
      *
               05  TBL-YOBO1           PIC X(01).
      *        データ区分
               05  TBL-DATAKBN         PIC X(01).
      *        （1:同日初診）
               05  TBL-YOBO2           PIC X(01).
      *        往診加算（0:なし,1:夜間、2:深夜・緊急）
      *        時間加算（0:なし,1:時間外、2:休日、3:深夜、4:特例
      *                 (5:乳幼児夜間、6:乳幼児休日、7:乳幼児深夜）
               05  TBL-KASAN           PIC X(01).
      *
      *
       01  TBL2-SYOKAI-AREA.
      *    小児科外来診察料(紹介加算)
           03  FILLER              PIC X(09)   VALUE   "111010370".
      *    初診時(紹介加算)
           03  FILLER              PIC X(09)   VALUE   "111003770".
      *    紹介加算１
           03  FILLER              PIC X(09)   VALUE   "113005070".
           03  FILLER              PIC X(09)   VALUE   "111011270".
      *    紹介加算２
           03  FILLER              PIC X(09)   VALUE   "113005170".
           03  FILLER              PIC X(09)   VALUE   "111011370".
      *    紹介加算３
           03  FILLER              PIC X(09)   VALUE   "111010870".
           03  FILLER              PIC X(09)   VALUE   "111009970".
      *    紹介加算４
           03  FILLER              PIC X(09)   VALUE   "111010970".
           03  FILLER              PIC X(09)   VALUE   "111010070".
      *    紹介加算５
           03  FILLER              PIC X(09)   VALUE   "111011070".
           03  FILLER              PIC X(09)   VALUE   "111010170".
      *    紹介加算６
           03  FILLER              PIC X(09)   VALUE   "111011170".
           03  FILLER              PIC X(09)   VALUE   "111010270".
      *
      *    育児栄養指導料
           03  FILLER              PIC X(09)   VALUE   "099114001".
           03  FILLER              PIC X(09)   VALUE   "111000470".
      *
       01  TBL2-SYOKAI-AREAR       REDEFINES   TBL2-SYOKAI-AREA.
           03  TBL2-SYOKAI-OCC         OCCURS  8     INDEXED  TBL2-IDX.
      *        小児科外来診察料の加算
               05  TBL2-SRYCD-1        PIC X(09).
      *        初診料の加算
               05  TBL2-SRYCD-2        PIC X(09).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1006.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    給付外点数編集パラメタ領域
           COPY    "CPORCSSYU40.INC".
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSSYU40AREA
           SYUNOU-REC
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
      *    診療区分別給付外点数クリア
           INITIALIZE                  SYU-KYUFUGAI
      *
           EVALUATE    ORCS40-KBN
               WHEN    "1"
      *            診療行為より
                   PERFORM 100-SRYKOU-GMN-SEC
               WHEN    "2"
      *            収納より
                   PERFORM 300-SYUNOU-GMN-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *   電子化加算チェック処理
      *****************************************************************
       1000-SYSKANRI-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "2"                 TO  SYS-1006-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索（施設基準）
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  WRK-DENSIKA
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SYS-1006-KBNCD      =   "2"
      *            電子化加算
                   IF      SYS-1006-SSTKIJUN (100) =   1
                       MOVE    1               TO  WRK-DENSIKA
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1000-SYSKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為作成処理
      *****************************************************************
       100-SRYKOU-GMN-SEC           SECTION.
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
      *    電子化加算
           IF      WRK-SRYYMD(1:6)     <   "200604"
               MOVE    ZERO            TO  WRK-DENSIKA
           ELSE
               PERFORM 1000-SYSKANRI-HEN-SEC
           END-IF
           MOVE    ZERO                TO  FLG-DOUJITU
           MOVE    ZERO                TO  FLG-SYOSIN
      *    当月最初の初診料算定日を判定する
           PERFORM 3000-SYOSIN-ALLCHK-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   5   )  OR
                              (FLG-SYOSIN-ERR  =   1   )   OR
                              (ORCS40-SRYKBN (IDX) =   SPACE)
               IF      ORCS40-SRYKBN (IDX)     =   "13"
                                               OR  "99"
      *            小児科外来診察料算定時、初診料計算
                   PERFORM 1001-SYOUNIKA-HEN-SEC
               ELSE
      *            小児科外来診察料以外
                   PERFORM 1002-SRYCD-CHK-SEC
               END-IF
           END-PERFORM
      *
      *    合計計算
           COMPUTE SYU-KYUFUGAI-GOKEI-TEN  =   SYU-KYUFUGAI-SHOSHIN-TEN
                                           +   SYU-KYUFUGAI-SIDOU-TEN
                                           +   SYU-KYUFUGAI-OSIN1-TEN
                                           +   SYU-KYUFUGAI-OSIN2-TEN
                                           +   SYU-KYUFUGAI-OSIN3-TEN
      *
      *TEST
      *    DISPLAY "SYU-SRYKA :" SYU-SRYKA
      *    DISPLAY "SYU-HKNCOMBINUM :" SYU-HKNCOMBINUM
      *    DISPLAY "SYU-KYUFUGAI-SHOSHIN-TEN:" SYU-KYUFUGAI-SHOSHIN-TEN
      *    DISPLAY "SYU-KYUFUGAI-SIDOU-TEN:" SYU-KYUFUGAI-SIDOU-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN1-TEN:" SYU-KYUFUGAI-OSIN1-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN2-TEN:" SYU-KYUFUGAI-OSIN2-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN3-TEN:" SYU-KYUFUGAI-OSIN3-TEN
      *TEST
           .
       100-SAKUSEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診察料以外処理
      *****************************************************************
       1002-SRYCD-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-KASAN
           MOVE    SPACE               TO  WRK-DOUJITU
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   5   )  OR
                              (ORCS40-SRYCD (IDX IDX2)
                                               =   SPACE)
      *        コードチェック
               SET     TBL-IDX             TO  1
               SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-OK2
                   WHEN    ORCS40-SRYCD (IDX IDX2)
                                               =   TBL-SRYCD (TBL-IDX)
                       IF      WRK-KASAN           =   ZERO
                           MOVE    TBL-KASAN (TBL-IDX) TO  WRK-KASAN
                       END-IF
                       IF      TBL-DATAKBN (TBL-IDX)   =   "1"
                           MOVE    1               TO  FLG-OK
                           MOVE    TBL-YOBO2 (TBL-IDX) TO  WRK-DOUJITU
                           MOVE    IDX2            TO  IDX-SYU
                       END-IF
               END-SEARCH
           END-PERFORM
           IF      FLG-OK              =   ZERO
               GO      TO      1002-SRYCD-CHK-EXT
           END-IF
      *
      *
      *    対象の診察料あり
           EVALUATE    ORCS40-SRYKBN (IDX)
               WHEN    "11"
      *            初診料の時、当月最初かどうかの判定
                   IF     (WRK-DD              =   ZERO )  OR
                          (WRK-SRYDD           <=  WRK-DD)
                       IF     (WRK-SRYDD           =   WRK-DD) AND
                              (FLG-SYOSIN          =   1     )
      *                    初診料と同日初診が同日にある場合、初診料のみ
                           IF      WRK-DOUJITU     =   SPACE
      *                    給付対象外点数（初診）
                               MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-SHOSHIN-TEN
                           END-IF
                       ELSE
      *                    給付対象外点数（初診）
                           MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-SHOSHIN-TEN
                       END-IF
                   END-IF
               WHEN    "14"
                   EVALUATE    WRK-KASAN
                       WHEN    "0"
      *                給付対象外点数（往診）
                           MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-OSIN1-TEN
                       WHEN    "1"
      *                給付対象外点数（往診／夜間）
                           MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-OSIN2-TEN
                       WHEN    "2"
      *                給付対象外点数（往診／深夜／緊急）
                           MOVE    ORCS40-ZAITEN (IDX)
                                           TO  SYU-KYUFUGAI-OSIN3-TEN
                   END-EVALUATE
           END-EVALUATE
      *
           .
       1002-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診察料算定処理
      *****************************************************************
       1001-SYOUNIKA-HEN-SEC            SECTION.
      *
      *    当月最初かどうかの判定
           IF     (WRK-DD              =   ZERO  )  OR
                  (WRK-SRYDD           <=  WRK-DD)
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-SYOSIN-ERR
               GO   TO      1001-SYOUNIKA-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-KASAN
           MOVE    SPACE               TO  WRK-HEN-SRYCD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   5   )  OR
                              (ORCS40-SRYCD (IDX IDX2)
                                               =   SPACE)
      *        コードチェック
               SET     TBL-IDX             TO  1
               SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-OK2
                   WHEN    ORCS40-SRYCD (IDX IDX2)
                                               =   TBL-SRYCD (TBL-IDX)
                       IF      WRK-KASAN           =   ZERO
                           MOVE    TBL-KASAN (TBL-IDX) TO  WRK-KASAN
                       END-IF
                       IF      TBL-DATAKBN (TBL-IDX)   =   "1"
                           MOVE    1               TO  FLG-OK
                       END-IF
                       MOVE    1               TO  FLG-OK2
               END-SEARCH
      *
               IF      FLG-OK2                 =   ZERO
      *            紹介加算の判定
                   SET     TBL2-IDX            TO  1
                   SEARCH  TBL2-SYOKAI-OCC     VARYING   TBL2-IDX
                       AT  END
                           MOVE    ZERO            TO  FLG-OK2
                       WHEN    ORCS40-SRYCD (IDX IDX2)
                                                   =   TBL2-SRYCD-1
                                                           (TBL2-IDX)
                            MOVE    TBL2-SRYCD-2 (TBL2-IDX)
                                                   TO  WRK-HEN-SRYCD
                   END-SEARCH
               END-IF
           END-PERFORM
      *    対象外
           IF     (FLG-OK              =   ZERO  )  AND
                  (WRK-HEN-SRYCD       =   SPACE )
               GO      TO      1001-SYOUNIKA-HEN-EXT
           END-IF
      *
      *    初診料点数を給付対象外点数へ
           IF      WRK-SRYYMD(1:6)     <   "200604"
               PERFORM 10013-SYOSIN-TEN-SEC
           ELSE
               PERFORM 10013-SYOSIN02-TEN-SEC
           END-IF
      *
           .
       1001-SYOUNIKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料点数を給付対象外点数設定処理
      *****************************************************************
       10013-SYOSIN-TEN-SEC            SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-ZAITEN
      *    診療コードから点数計算
           IF      FLG-OK              =   1
      *        診察料
               IF      SPA-HOSPSBT         =   "1"
      *            病院
                   MOVE    "111000110"         TO  WRK-SRYCD
               ELSE
      *            診療所
                   MOVE    "111003610"         TO  WRK-SRYCD
               END-IF
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
      *
      *        乳幼児加算
               IF      WRK-KASAN           =   ZERO
                   MOVE    "111000370"         TO  WRK-SRYCD
               ELSE
                   MOVE    "111011470"         TO  WRK-SRYCD
               END-IF
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
      *
      *        時間外
               EVALUATE    WRK-KASAN
                   WHEN    "1"
      *                時間外
                       MOVE    "111000570"         TO  WRK-SRYCD
                   WHEN    "2"
      *                休日
                       MOVE    "111000670"         TO  WRK-SRYCD
                   WHEN    "3"
      *                深夜
                       MOVE    "111000770"         TO  WRK-SRYCD
                   WHEN    "4"
      *                時間外特例休日
                       MOVE    "111000870"         TO  WRK-SRYCD
                   WHEN    "5"
      *                乳幼児夜間
                       MOVE    "111011570"         TO  WRK-SRYCD
                   WHEN    "6"
      *                乳幼児休日
                       MOVE    "111011670"         TO  WRK-SRYCD
                   WHEN    "7"
      *                乳幼児深夜
                       MOVE    "111011770"         TO  WRK-SRYCD
                   WHEN    OTHER
      *                時間外なし
                       MOVE    SPACE               TO  WRK-SRYCD
               END-EVALUATE
               IF      WRK-SRYCD       NOT =   SPACE
                   PERFORM 10011-TENSU-SET-SEC
                   ADD     WRK-TENSU               TO  WRK-ZAITEN
               END-IF
           END-IF
      *
           IF      WRK-HEN-SRYCD   NOT =   SPACE
               MOVE    WRK-HEN-SRYCD       TO  WRK-SRYCD
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
           END-IF
      *
      *    給付対象外点数（指導）
           ADD     WRK-ZAITEN         TO  SYU-KYUFUGAI-SIDOU-TEN
      *
           .
       10013-SYOSIN-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料点数を給付対象外点数設定処理
      *****************************************************************
       10013-SYOSIN02-TEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAITEN
      *    診療コードから点数計算
           IF      FLG-OK              =   1
      *        病院
               MOVE    "111000110"         TO  WRK-SRYCD
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
      *
      *        乳幼児加算
               MOVE    SPACE                   TO  WRK-SRYCD
      *        時間外
               EVALUATE    WRK-KASAN
                   WHEN    "1"
      *                時間外
                       MOVE    "111011970"         TO  WRK-SRYCD
                   WHEN    "2"
      *                休日
                       MOVE    "111012070"         TO  WRK-SRYCD
                   WHEN    "3"
      *                深夜
                       MOVE    "111012170"         TO  WRK-SRYCD
                   WHEN    "4"
      *                時間外特例休日
                       MOVE    "111012270"         TO  WRK-SRYCD
                   WHEN    "5"
      *                乳幼児夜間
                       MOVE    "111011570"         TO  WRK-SRYCD
                   WHEN    "6"
      *                乳幼児休日
                       MOVE    "111011670"         TO  WRK-SRYCD
                   WHEN    "7"
      *                乳幼児深夜
                       MOVE    "111011770"         TO  WRK-SRYCD
                   WHEN    "8"
      *                乳幼児夜間早朝等
                       MOVE    "111012470"         TO  WRK-SRYCD
                   WHEN    OTHER
      *                時間外なし
                       MOVE    "111000370"         TO  WRK-SRYCD
               END-EVALUATE
               IF      WRK-SRYCD       NOT =   SPACE
                   PERFORM 10011-TENSU-SET-SEC
                   ADD     WRK-TENSU               TO  WRK-ZAITEN
               END-IF
      *        電子化加算
               IF      WRK-DENSIKA         =   1
                   MOVE    "111012370"         TO  WRK-SRYCD
                   PERFORM 10011-TENSU-SET-SEC
                   ADD     WRK-TENSU               TO  WRK-ZAITEN
               END-IF
           END-IF
      *
           IF      WRK-HEN-SRYCD   NOT =   SPACE
               MOVE    WRK-HEN-SRYCD       TO  WRK-SRYCD
               PERFORM 10011-TENSU-SET-SEC
               ADD     WRK-TENSU               TO  WRK-ZAITEN
           END-IF
      *
      *    給付対象外点数（指導）
           ADD     WRK-ZAITEN         TO  SYU-KYUFUGAI-SIDOU-TEN
      *
           .
       10013-SYOSIN02-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為点数設定処理
      *****************************************************************
       10011-TENSU-SET-SEC            SECTION.
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-TEN             TO  WRK-TENSU
           ELSE
               MOVE    ZERO                TO  WRK-TENSU
           END-IF
      *
           .
       10011-TENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納より作成処理
      *****************************************************************
       300-SYUNOU-GMN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOSIN-ERR
      *    受診歴を検索する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    ORCS40-DENPNUM      TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *    診察料に入力がある時のみ対象とする
           IF     (FLG-JYURRK          =   ZERO )  AND
                  (JYURRK-RENNUM       =   1    )  AND
                  (JYURRK-SRYKBN1  NOT =   SPACE)
               MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
      *        電子化加算
               IF      WRK-SRYYMD(1:6)     <   "200604"
                   MOVE    ZERO            TO  WRK-DENSIKA
               ELSE
                   PERFORM 1000-SYSKANRI-HEN-SEC
               END-IF
      *        当月最初の初診料算定日を判定する
               PERFORM 3000-SYOSIN-ALLCHK-SEC
      *****ELSE
      *********MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           PERFORM
                   UNTIL   (FLG-JYURRK     =   1 )  OR
                           (FLG-SYOSIN-ERR =   1 )
               PERFORM VARYING     IDX-Z   FROM    1  BY  1
                       UNTIL       IDX-Z   >   15
                   IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
      *                診療会計取得
                       PERFORM 3001-SRYACCT-CHK-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    合計計算
           COMPUTE SYU-KYUFUGAI-GOKEI-TEN  =   SYU-KYUFUGAI-SHOSHIN-TEN
                                           +   SYU-KYUFUGAI-SIDOU-TEN
                                           +   SYU-KYUFUGAI-OSIN1-TEN
                                           +   SYU-KYUFUGAI-OSIN2-TEN
                                           +   SYU-KYUFUGAI-OSIN3-TEN
      *
      *TEST
      *    DISPLAY "SYU-KYUFUGAI-SHOSHIN-TEN:" SYU-KYUFUGAI-SHOSHIN-TEN
      *    DISPLAY "SYU-KYUFUGAI-SIDOU-TEN:" SYU-KYUFUGAI-SIDOU-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN1-TEN:" SYU-KYUFUGAI-OSIN1-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN2-TEN:" SYU-KYUFUGAI-OSIN2-TEN
      *    DISPLAY "SYU-KYUFUGAI-OSIN3-TEN:" SYU-KYUFUGAI-OSIN3-TEN
      *TEST
           .
       300-SYUNOU-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月最初の初診料算定日を判定処理
      *****************************************************************
       3000-SYOSIN-ALLCHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-DOUJITU
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL      (FLG-SANTEI      =   1  )
      *D               OR (FLG-ERR         =   1  )
      *
               IF      SANTEI-MSANTEICNT   >   ZERO
      *            当月算定分
                   PERFORM 30001-TOUGETU-CHK-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    処理日に初診料算定
           IF      WRK-DD              =   WRK-SRYDD
               MOVE    1               TO  FLG-SYOSIN
           END-IF
      *    同日初診編集
           IF      WRK-DD2             >   ZERO
               IF     (WRK-DD          >   WRK-DD2 )
                  OR  (WRK-DD          =   ZERO    )
                   MOVE    WRK-DD2         TO  WRK-DD
                   MOVE    ZERO            TO  FLG-SYOSIN
               END-IF
           END-IF
           .
       3000-SYOSIN-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月算定分チェック処理
      *****************************************************************
       30001-TOUGETU-CHK-SEC            SECTION.
      *
           EVALUATE    SANTEI-SRYCD
      *        初診料
               WHEN    "111000110"
               WHEN    "111700110"
               WHEN    "111003610"
               WHEN    "111700210"
      *        小児科外来診療科
               WHEN    "113003510"
               WHEN    "113003710"
      *        ダミー
               WHEN    "099110001"
      *            最小の算定日編集
                   IF     (WRK-DD              =   ZERO )  OR
                          (SANTEI-MSANTEID     <   WRK-DD)
                       MOVE    SANTEI-MSANTEID     TO  WRK-DD
                   END-IF
                   IF      WRK-DD              <   WRK-SRYDD
                        MOVE    1              TO  FLG-ERR
                   END-IF
      *        同日複数診療科
               WHEN    "111011810"
      *H25.4   同日初診（紹介状なし）
               WHEN    "111012610"
      *            最小の算定日編集
                   IF     (WRK-DD2             =   ZERO )  OR
                          (SANTEI-MSANTEID     <   WRK-DD2)
                       MOVE    SANTEI-MSANTEID     TO  WRK-DD2
                   END-IF
           END-EVALUATE
      *
           .
       3000-SYOSIN-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       3001-SRYACCT-CHK-SEC            SECTION.
     *
           MOVE    ZERO                TO  FLG-SYORI-END
     *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-Z)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL  (FLG-SRYACCT          =   1 )  OR
                      (FLG-SYORI-END        =   1 )
      *
               MOVE    JYURRK-SRYYMD(7:2)   TO  IDX-D2
               COMPUTE IDX-D           =   JYURRK-RENNUM   +   1
               IF     (ACCT-SRYKBN     =   "11"  OR  "13"  OR
                                           "14"  OR   "99"  ) AND
                      (ACCT-DAY (IDX-D IDX-D2)   >   ZERO   )
                   PERFORM 30011-SRYACT-SYORI-SEC
               END-IF
      *
               IF      FLG-SYORI-END       =   ZERO
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為チェック処理
      *****************************************************************
       30011-SRYACT-SYORI-SEC            SECTION.
     *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-KASAN
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
                MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5        )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE)
      *            手技料の時、判定する
                   IF      SRY-SRYCD (IDZ)(1:1)    =   "1"
                       PERFORM 300111-TBL-CHK-SEC
                   END-IF
                   IF     (ACCT-SRYKBN             =   "99")  AND
                          (SRY-SRYCD (IDZ)         =   "099114001")
                       PERFORM 300111-TBL-CHK-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-OK              =   ZERO )  AND
                  (WRK-KASAN           =   SPACE)
               GO      TO      30011-SRYACT-SYORI-EXT
           ELSE
      *        対象があった時、編集する
               PERFORM 30012-KYUFUGAI-HEN-SEC
           END-IF
      *
           .
       30011-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象診療コードチェック処理
      *****************************************************************
       300111-TBL-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-DOUJITU
      *    コードチェック
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
               AT  END
                   MOVE    ZERO            TO  FLG-OK2
               WHEN    SRY-SRYCD (IDZ)     =   TBL-SRYCD (TBL-IDX)
                   IF      WRK-KASAN           =   ZERO
                       MOVE    TBL-KASAN (TBL-IDX) TO  WRK-KASAN
                   END-IF
                   IF      TBL-DATAKBN (TBL-IDX)   =   "1"
                       MOVE    1               TO  FLG-OK
                       MOVE    TBL-YOBO2 (TBL-IDX) TO  WRK-DOUJITU
                       MOVE    TBL-IDX         TO  IDX-SYU
                   END-IF
                   MOVE    1               TO  FLG-OK2
           END-SEARCH
      *
           IF      FLG-OK                  =   1
      *        同日初診コードで同日に初診料がないこと
               IF      WRK-DOUJITU         =   "1"
                   IF      FLG-SYOSIN      =   1
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-OK2                 =   ZERO
      *        紹介加算の判定
               SET     TBL2-IDX            TO  1
               SEARCH  TBL2-SYOKAI-OCC     VARYING   TBL2-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-OK2
                   WHEN    SRY-SRYCD (IDZ)     =   TBL2-SRYCD-1
                                                           (TBL2-IDX)
                        MOVE    TBL2-SRYCD-2 (TBL2-IDX)
                                                   TO  WRK-HEN-SRYCD
               END-SEARCH
           END-IF
      *
           .
       300111-TBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    給付対象外点数編集処理
      *****************************************************************
       30012-KYUFUGAI-HEN-SEC            SECTION.
      *
      *    対象の診察料あり
           EVALUATE    ACCT-SRYKBN  ALSO   FLG-OK
               WHEN    "11"         ALSO   1
                   MOVE    1                   TO  FLG-SYORI-END
      *            初診料の時、当月最初かどうかの判定
                   IF     (WRK-DD              =   ZERO )  OR
                          (WRK-SRYDD           <=  WRK-DD)
      *                給付対象外点数（初診）
                       MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-SHOSHIN-TEN
      *********    ELSE
      *********        MOVE    1           TO  FLG-SYOSIN-ERR
                   END-IF
               WHEN    "14"         ALSO   1
                   MOVE    1                   TO  FLG-SYORI-END
                   EVALUATE    WRK-KASAN
                       WHEN    "0"
      *                給付対象外点数（往診）
                           MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-OSIN1-TEN
                       WHEN    "1"
      *                給付対象外点数（往診／夜間）
                           MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-OSIN2-TEN
                       WHEN    "2"
      *                給付対象外点数（往診／深夜／緊急）
                           MOVE    ACCT-ZAITEN
                                           TO  SYU-KYUFUGAI-OSIN3-TEN
                   END-EVALUATE
               WHEN    "13"        ALSO    1
      *            初診料の時、当月最初かどうかの判定
                   IF     (WRK-DD              =   ZERO )  OR
                          (WRK-SRYDD           <=  WRK-DD)
      *                初診料点数を給付対象外点数へ
                       IF      WRK-SRYYMD(1:6)     <   "200604"
                           PERFORM 10013-SYOSIN-TEN-SEC
                       ELSE
                           PERFORM 10013-SYOSIN02-TEN-SEC
                       END-IF
                       MOVE    1                   TO  FLG-SYONIKA
                   ELSE
                       MOVE    1                   TO  FLG-SYORI-END
                   END-IF
               WHEN    "99"        ALSO    ANY
      *            小児科外来診察料
      *            初診料点数を給付対象外点数へ
                   IF      FLG-SYONIKA         =   1
      *                初診料点数を給付対象外点数へ
                       IF      WRK-SRYYMD(1:6)     <   "200604"
                           PERFORM 10013-SYOSIN-TEN-SEC
                       ELSE
                           PERFORM 10013-SYOSIN02-TEN-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       30012-KYUFUGAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-RAED-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-READ-SEC           SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
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
      *
      *****************************************************************
      *    ＤＢクローズ処理
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
      *
