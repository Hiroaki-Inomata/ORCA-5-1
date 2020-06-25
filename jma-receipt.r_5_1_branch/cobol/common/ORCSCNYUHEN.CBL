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
       PROGRAM-ID.                 ORCSCNYUHEN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 入院履歴情報編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/11/06    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/10/26  他院退院日対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *
           03  FLG-SYORI-END       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-D2              PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SYMD                PIC X(08).
           03  WRK-OLD-NYUINYMD        PIC X(08).
           03  WRK-OLD-TAIINYMD        PIC X(08).
      *
           03  WRK-OLD-RRKNUM          PIC 9(03).
      *
           03  WRK-TAI-TAIINYMD        PIC X(08).
      *
           03  WRK-TAIINYMD            PIC X(08).
           03  WRK-LASTTAIINYMD        PIC X(08).
           03  WRK-YMD.
               05  WRK-SYM             PIC 9(06).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-SHONUM                  PIC 9(03).
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
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCNYUHEN.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCNYUHENAREA
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
      *
           INITIALIZE                  ORCSCNYUHEN-OUT-AREA
      *
           IF      ORCSCNYUHEN-SRYYMD  =   SPACE
               MOVE    SPA-SYSYMD      TO   ORCSCNYUHEN-SRYYMD
           END-IF
           IF      ORCSCNYUHEN-KBN     =   SPACE
               MOVE    "1"             TO  ORCSCNYUHEN-KBN
           END-IF
      *
      *    入院履歴編集処理
           PERFORM 100-MAIN-SYORI-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    入院履歴編集処理
      *****************************************************************
       100-MAIN-SYORI-SEC              SECTION.
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *    入院が１件もないとき
           IF      FLG-PTNYUINRRK      =   1
               MOVE    ZERO                TO  ORCSCNYUHEN-RRK-NYUIN
           ELSE
               MOVE    1                   TO  ORCSCNYUHEN-RRK-NYUIN
           END-IF
      *
           MOVE    ORCSCNYUHEN-SRYYMD  TO  WRK-SRYYMD
           MOVE    ZERO                TO  FLG-SYORI-END
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-OLD-NYUINYMD
           MOVE    SPACE               TO  WRK-OLD-TAIINYMD
           MOVE    SPACE               TO  WRK-TAI-TAIINYMD
           MOVE    ZERO                TO  WRK-OLD-RRKNUM
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )
                          OR  (FLG-SYORI-END       =   1 )
      *      外来の判定は、自院歴分は対象外
             IF     ((SPA-NYUGAIKBN        =   "2" )
                 AND (PTNYUINRRK-JTIKBN    =   "6" ))  
      *              他院入院は対象外
               OR    (PTNYUINRRK-JTIKBN    =   "5" )
               CONTINUE
             ELSE
               EVALUATE    ORCSCNYUHEN-KBN
      *            診療日付での判定
                   WHEN    "1"
                       PERFORM 1001-SRYYMD-SYORI-SEC
      *            診療月での判定
                   WHEN    "2"
                       PERFORM 1002-SRYYM-SYORI-SEC
               END-EVALUATE
      *        最終退院日
               IF     (WRK-LASTTAIINYMD        =   SPACE      )  OR
                      (PTNYUINRRK-TAIINYMD     >   WRK-LASTTAIINYMD)
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-LASTTAIINYMD
               END-IF
      *
      *        退院月が当月以前で終了とする
               IF     (PTNYUINRRK-TAIINYMD(1:6)    <   WRK-SRYYMD(1:6))
                   MOVE    1                   TO  FLG-SYORI-END
               END-IF
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-OLD-NYUINYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  WRK-OLD-TAIINYMD
               MOVE    PTNYUINRRK-RRKNUM   TO  WRK-OLD-RRKNUM
           END-IF
      *
      *       他院退院日編集
               IF      (SPA-NYUGAIKBN       =   "2" )
                AND    (PTNYUINRRK-JTIKBN   =   "5" )
                   IF     (PTNYUINRRK-TAIINYMD <   WRK-SRYYMD )
                     AND ((WRK-TAI-TAIINYMD    =   SPACE     )   OR
                          (WRK-TAI-TAIINYMD    <   PTNYUINRRK-TAIINYMD))
                       MOVE    PTNYUINRRK-TAIINYMD  TO  WRK-TAI-TAIINYMD
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    最終退院日の設定
           MOVE    WRK-TAIINYMD        TO  ORCSCNYUHEN-MAE-TAIINYMD
           MOVE    WRK-LASTTAIINYMD    TO  ORCSCNYUHEN-LAST-TAIINYMD
      *    他医院退院日
           MOVE    WRK-TAI-TAIINYMD    TO  ORCSCNYUHEN-TAI-TAIINYMD
      *
      *    該当の入院履歴が継続の時
           MOVE    SPACE               TO  ORCSCNYUHEN-STR-NYUINYMD
           IF      WRK-SHONUM      NOT =   ZERO
               PERFORM 1003-STR-NYUINYMD-HEN-SEC
           END-IF
           .
       100-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療日付での判定処理
      *****************************************************************
       1001-SRYYMD-SYORI-SEC            SECTION.
      *
      *    前回退院日＝入院日の時、対象とする
           IF    ((PTNYUINRRK-NYUINYMD NOT =   WRK-OLD-NYUINYMD ) AND
                  (PTNYUINRRK-TAIINYMD NOT =   WRK-OLD-TAIINYMD ))
      *H26.12     連番毎に対象
             OR  ( PTNYUINRRK-RRKNUM   NOT =   WRK-OLD-RRKNUM   )
      *
               IF      PTNYUINRRK-TAIINYMD     =   WRK-OLD-NYUINYMD
                   IF      (ORCSCNYUHEN-KBN2   NOT =   "1"     )
                       IF    ( FLG-OK          =   1             )
                       AND   ( PTNYUINRRK-TAIINYMD =   WRK-SRYYMD)
                           MOVE    1           TO  ORCSCNYUHEN-DOUNYUKBN
                       END-IF
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               END-IF
      *!!!     同日再入院日あり対象
               IF     (PTNYUINRRK-TAIINYMD     =   WRK-OLD-NYUINYMD )
                 AND  (WRK-OLD-NYUINYMD        =   ORCSCNYUHEN-NYUINYMD)
      *            同日再入院日
                   MOVE    PTNYUINRRK-TAIINYMD    TO  WRK-YMD
                   MOVE    WRK-SDD         TO  ORCSCNYUHEN-DOUNYUIN 
               END-IF
           END-IF
      *
      *
      *    当日以前の転入日
           IF      PTNYUINRRK-TENNYUYMD    <=  WRK-SRYYMD
      *        転出日
               IF      PTNYUINRRK-TENSTUYMD    >=  WRK-SRYYMD
      *            当日入院中
                   IF    ((PTNYUINRRK-TAIINYMD     =   WRK-SRYYMD)
                     OR   (PTNYUINRRK-TENSTUYMD    >   WRK-SRYYMD))
                     AND  (FLG-OK                  =   ZERO      )
                       PERFORM 1002-PTNYUINRRK-HEN-SEC
                       MOVE    1               TO  FLG-OK
      *                同日再入院日
                       MOVE    ZERO            TO  ORCSCNYUHEN-DOUNYUIN
                   END-IF
               END-IF
           END-IF
      *    入院期間で１回
           IF     (PTNYUINRRK-NYUINYMD NOT =   WRK-OLD-NYUINYMD ) AND
      ************(PTNYUINRRK-NYUINYMD     <=  WRK-SRYYMD       )
                  (PTNYUINRRK-NYUINYMD(1:6)    <=  WRK-SRYYM    )
               IF     (PTNYUINRRK-NYUINYMD(1:6)
                                           <=  WRK-SRYYM  ) AND
                      (PTNYUINRRK-TAIINYMD (1:6)
                                           >=  WRK-SRYYM  )
      *            当月の入院期間設定
                   PERFORM 1003-NYUINDAY-HEN-SEC
               END-IF
      *        診療日以前の最終退院日
               IF     (PTNYUINRRK-TAIINYMD     <=  WRK-SRYYMD ) AND
                      (WRK-TAIINYMD            =   SPACE      )
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-TAIINYMD
               END-IF
           END-IF
           .
       1001-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療月での判定処理
      *****************************************************************
       1002-SRYYM-SYORI-SEC            SECTION.
      *
      *    当日以前の転入日
           IF      PTNYUINRRK-TENNYUYMD(1:6)  <=  WRK-SRYYM
      *        当月転出日
               IF     (PTNYUINRRK-TENSTUYMD(1:6)   >=  WRK-SRYYM )
                 AND  (FLG-OK                  =   ZERO      )
                   PERFORM 1002-PTNYUINRRK-HEN-SEC
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-IF
      *    当月の入院期間設定
      *    入院期間で１回
           IF     (PTNYUINRRK-NYUINYMD     NOT =   WRK-OLD-NYUINYMD) AND
                  (PTNYUINRRK-NYUINYMD(1:6)    <=  WRK-SRYYM   )
               IF     (PTNYUINRRK-NYUINYMD(1:6)
                                               <=  WRK-SRYYM ) AND
                      (PTNYUINRRK-TAIINYMD(1:6)
                                               >=  WRK-SRYYM )
                   PERFORM 1003-NYUINDAY-HEN-SEC
               END-IF
      *        診療日以前の最終退院日
               IF     (WRK-TAIINYMD            =   SPACE      )  OR
                      (WRK-TAIINYMD            <   PTNYUINRRK-TAIINYMD )
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-TAIINYMD
               END-IF
           END-IF
           .
       1002-SRYYM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院内容編集処理
      *****************************************************************
       1002-PTNYUINRRK-HEN-SEC            SECTION.
      *
      *    入院・退院日
           MOVE    PTNYUINRRK-NYUINYMD     TO  ORCSCNYUHEN-NYUINYMD
           MOVE    PTNYUINRRK-TAIINYMD     TO  ORCSCNYUHEN-TAIINYMD
      *    入院科
           MOVE    PTNYUINRRK-NYUINKA      TO  ORCSCNYUHEN-NYUINKA
      *    保険組合せ番号
           MOVE    PTNYUINRRK-HKNCOMBINUM  TO  ORCSCNYUHEN-HKNCOMBI
      *    担当医１（主治医）
           MOVE    PTNYUINRRK-DRCD1        TO  ORCSCNYUHEN-DRCD
      *
           IF     (PTNYUINRRK-TAIINYMD     =   WRK-SRYYMD)
               OR (PTNYUINRRK-NYUINYMD     =   WRK-SRYYMD)
      *           当日入退院
               MOVE    2                   TO  ORCSCNYUHEN-NYUIN-KBN
           ELSE
      *        入院中
               MOVE    1                   TO  ORCSCNYUHEN-NYUIN-KBN
           END-IF
      *ver.4.7
      *    継続
           IF      PTNYUINRRK-SHOKAIKBN    =   "2"
      *        継続
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
           ELSE
               MOVE    ZERO                TO  WRK-SHONUM
           END-IF
           IF      PTNYUINRRK-TAIINYMD =   WRK-OLD-NYUINYMD
      *        退院日＝前回入院日あり
               MOVE    1               TO  ORCSCNYUHEN-DOUTAIKBN
           END-IF
           .
       1002-PTNYUINRRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   当月入院日編集処理
      *****************************************************************
       1003-NYUINDAY-HEN-SEC            SECTION.
      *
           IF      PTNYUINRRK-TAIINYMD(1:6)
                                       =   WRK-SRYYMD(1:6)
               MOVE    PTNYUINRRK-TAIINYMD(7:2)
                                           TO  WRK-EDDD
           ELSE
               MOVE    31                  TO  WRK-EDDD
           END-IF
           IF      PTNYUINRRK-NYUINYMD(1:6)
                                       =   WRK-SRYYMD(1:6)
               MOVE    PTNYUINRRK-NYUINYMD(7:2)
                                           TO  WRK-STDD
           ELSE
               MOVE    01                  TO  WRK-STDD
           END-IF
      *    入院中
           PERFORM VARYING IDX-D2      FROM    WRK-STDD BY  1
                   UNTIL   IDX-D2      >   WRK-EDDD
               IF    ((PTNYUINRRK-TENNYUYMD(1:6)
                                           =   WRK-SRYYM )  AND
                      (IDX-D2              =   WRK-STDD  ))
               OR    ((PTNYUINRRK-TENSTUYMD(1:6)
                                           =   WRK-SRYYM )  AND
                      (IDX-D2              =   WRK-EDDD  ))
      *            入退院日
                   MOVE    2           TO  ORCSCNYUHEN-NYUDAY (IDX-D2)
               ELSE
      *            入院中
                   MOVE    1           TO  ORCSCNYUHEN-NYUDAY (IDX-D2)
               END-IF
           END-PERFORM
           .
       1003-NYUINDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴が継続開始編集処理
      *****************************************************************
       1003-STR-NYUINYMD-HEN-SEC             SECTION.
      *
      *    継続の時、初回入院日検索
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    ORCSCNYUHEN-NYUINYMD    TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-SHONUM          TO  PTNYUINRRK-SHONUM
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
            ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           IF      FLG-PTNYUINRRK      =   ZERO
               MOVE    PTNYUINRRK-NYUINYMD
                                           TO  ORCSCNYUHEN-STR-NYUINYMD
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1003-STR-NYUINYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
