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
       PROGRAM-ID.             ORCSPTHKNRRK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者保険確認年月日履歴更新処理
      *  返却エラーコード  : 00:正常　09:更新エラー　01:対象なし
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11.08.04    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
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
           03  FLG-PTHKNRRK        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-RENNUM               PIC 9(02).
      *
      *    保険確認年月日履歴テーブル
       01  TBL-PTHKNRRK-AREA.
           02  TBL-PTHKNRRK-REC             OCCURS   10.
           COPY    "CPPTHKNRRK.INC"  REPLACING
                                     //PTHKNRRK-// BY //TBL-PTHKNRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険確認年月日履歴マスタ
       01  PTHKNRRK-REC.
           COPY    "CPPTHKNRRK.INC".
      *
           COPY    "MCPAREA".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    保険確認年月日履歴登録サブ
           COPY    "CPORCSPTHKNRRK.INC".
      *
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA
           ORCSPTHKNRRKAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SYORI-SEC
      *
           IF      ORCSHKNRRK-RC       =   ZERO
      *        主　処理
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
           .
      *
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SYORI-SEC                SECTION.
      *
           IF   (ORCSHKNRRK-PTID       =   ZERO )
           OR   (ORCSHKNRRK-HKNID      =   ZERO )
           OR   (ORCSHKNRRK-KAKUNINYMD =   SPACE
                                       OR  ZERO  )
               MOVE    1                   TO  ORCSHKNRRK-RC
           END-IF
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SYORI-SEC                SECTION.
      *
           INITIALIZE                  TBL-PTHKNRRK-AREA
      *
           INITIALIZE                  PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    ORCSHKNRRK-PTID     TO  PTHKNRRK-PTID
           MOVE    ORCSHKNRRK-HKNID    TO  PTHKNRRK-HKNID
      *
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNRRK
               INITIALIZE                  PTHKNRRK-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL     (FLG-PTHKNRRK     =   1    )
               ADD     1                   TO  IDX
               MOVE    PTHKNRRK-REC        TO  TBL-PTHKNRRK-REC (IDX)
      *
               MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     ORCSHKNRRK-KAKUNINYMD    =   TBL-PTHKNRRK-KAKUNINYMD
                                                                 (1)
      *        同日の更新なし
               CONTINUE
           ELSE
      *        確認年月日登録処理
               PERFORM 2001-PTHKNRRK-SYORI-SEC
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険確認年月日履歴処理
      *****************************************************************
       2001-PTHKNRRK-SYORI-SEC              SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    確認年月日履歴削除
           INITIALIZE                  PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    ORCSHKNRRK-PTID     TO  PTHKNRRK-PTID
           MOVE    ORCSHKNRRK-HKNID    TO  PTHKNRRK-HKNID
      *
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSPTHKNRRK DEL ERR:" PTHKNRRK-KEY
                                    ",MCP-RC:" MCP-RC
               MOVE    9                   TO  ORCSHKNRRK-RC
               GO      TO      2001-PTHKNRRK-SYORI-EXT
           END-IF
      *
      *    今回分追加
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM 20011-PTHKNRRK-ADD-SEC
      *    前回分追加
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5    )
                   OR         (ORCSHKNRRK-RC   NOT =   ZERO )
                   OR         (TBL-PTHKNRRK-KAKUNINYMD (IDX) =   SPACE)
               MOVE    TBL-PTHKNRRK-REC (IDX)  TO  PTHKNRRK-REC
      *
               ADD     1                   TO  WRK-RENNUM
               IF      WRK-RENNUM          >   5
      *            登録件数は５件までとする
                   MOVE    10                  TO  IDX
               ELSE
                   MOVE    WRK-RENNUM          TO  PTHKNRRK-RENNUM
      *
                   MOVE    SMCNDATE-YMD        TO  PTHKNRRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  PTHKNRRK-UPHMS
                   PERFORM 20013-PTHKNRRK-INS-SEC
               END-IF
           END-PERFORM
           .
       2001-PTHKNRRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回分保険確認年月日履歴登録処理
      *****************************************************************
       20011-PTHKNRRK-ADD-SEC              SECTION.
      *
           INITIALIZE                  PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    ORCSHKNRRK-PTID     TO  PTHKNRRK-PTID
           MOVE    ORCSHKNRRK-HKNID    TO  PTHKNRRK-HKNID
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  PTHKNRRK-RENNUM
           MOVE    ORCSHKNRRK-KAKUNINYMD
                                       TO  PTHKNRRK-KAKUNINYMD
      *
           MOVE    SMCNDATE-YMD        TO  PTHKNRRK-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTHKNRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTHKNRRK-UPHMS
           PERFORM 20013-PTHKNRRK-INS-SEC
           .
       20011-PTHKNRRK-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険確認年月日履歴登録処理
      *****************************************************************
       20013-PTHKNRRK-INS-SEC          SECTION.
      *
           MOVE    SPA-OPID            TO  PTHKNRRK-OPID
      *
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSPTHKNRRK INS ERR:" PTHKNRRK-KEY
                                    ",MCP-RC:" MCP-RC
               MOVE    9                   TO  ORCSHKNRRK-RC
           END-IF
           .
       20013-PTHKNRRK-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険確認年月日履歴マスタ検索処理
      *****************************************************************
       900-PTHKNRRK-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNRRK-REC
               MOVE    ZERO                TO  FLG-PTHKNRRK
           ELSE
               INITIALIZE                  PTHKNRRK-REC
               MOVE    1                   TO  FLG-PTHKNRRK
           END-IF
           .
       900-PTHKNRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
