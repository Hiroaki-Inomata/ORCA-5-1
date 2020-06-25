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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGZS02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ用点数付加内容更新（ＺＳ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/02    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/12/09  MONFUNC対応
      *  03.04.00    NACL-多々納  06/12/05  上限回数追加、パラメタ変更他
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.04.00    NACL-多々納  09/01/05  前回検査日区分追加
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(02).
           03  IDX1                              PIC 9(02).
           03  IDX2                              PIC 9(02).
           03  IDX-H                             PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-CDSYU               PIC X(01).
           03  WRK-KETA                PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    ワーク点数マスタ付加コード
        01 TBL-TENSUPLUS-AREA.
          02 TBL-TENSUPLUS-REC         OCCURS  30.
           COPY    "CPTENSUPLUS.INC"   REPLACING
                                       //TNSP-// BY //TBL-TNSP-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *    点数附加情報更新内容
           COPY    "CPORCGZS02.INC".
      *
        01 LNK-TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC"   REPLACING
                                       //TNSP-// BY //LNK-TNSP-//.
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 ORCGZS02AREA
                                                 LNK-TENSUPLUS-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    点数マスタ付加情報と点数マスタの範囲チェック
           PERFORM 100-TENSUPLUS-CHK-SEC
      *
      *    点数マスタ付加情報更新
           PERFORM 200-TENSUPLUS-UPD-SEC
      *
           MOVE    SPA-AREA                      TO  SPA-COMMON
      *
           EXIT    PROGRAM
           .
       000-PROC-EXT.
           EXIT.
      *
      *****************************************************************
      *  点数マスタ付加情報チェック処理
      *****************************************************************
       100-TENSUPLUS-CHK-SEC          SECTION.
      *
           INITIALIZE                      TBL-TENSUPLUS-AREA
           MOVE    ZERO                TO  IDX-H
      *
           INITIALIZE                      TENSUPLUS-REC
           MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           PERFORM UNTIL   FLG-TENSUPLUS   =   1
               ADD     1                   TO  IDX-H
               MOVE    TENSUPLUS-REC       TO  TBL-TENSUPLUS-REC (IDX-H)
      *
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      IDX-H               =   ZERO
               GO      TO      100-TENSUPLUS-CHK-EXT
           END-IF
      *
      *    点数マスタと期間を合わせる
           MOVE    1                   TO  IDX1
           MOVE    1                   TO  IDX2
           PERFORM UNTIL     (IDX1     >   IDX-H  )
                        AND  (IDX2     >   SPA-Z01-MAX )
               IF       IDX2           >   SPA-Z01-MAX
      *             附加のみ削除
                       MOVE    TBL-TENSUPLUS-REC (IDX1)
                                                   TO  TENSUPLUS-REC
                       MOVE    TNSP-SRYCD          TO  TNSP-UP-SRYCD
                       MOVE    TNSP-YUKOSTYMD      TO  TNSP-UP-YUKOSTYMD
                       MOVE    TNSP-YUKOEDYMD      TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                    ADD     1               TO  IDX1
               ELSE
      *
               EVALUATE    TRUE
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                       =   TBL-TNSP-YUKOSTYMD(IDX1)) AND
                           (SPA-Z01-TYUKOEDYMD(IDX2)
                                       =   TBL-TNSP-YUKOEDYMD(IDX1))
                       ADD     1               TO  IDX1
                       ADD     1               TO  IDX2
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                       =   TBL-TNSP-YUKOSTYMD(IDX1)) AND
                           (SPA-Z01-TYUKOEDYMD(IDX2)
                                   NOT =   TBL-TNSP-YUKOEDYMD(IDX1))
      *                終了日変更
                       MOVE    TBL-TENSUPLUS-REC (IDX1)
                                                   TO  TENSUPLUS-REC
                       MOVE    TNSP-SRYCD          TO  TNSP-UP-SRYCD
                       MOVE    TNSP-YUKOSTYMD      TO  TNSP-UP-YUKOSTYMD
                       MOVE    TNSP-YUKOEDYMD      TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
      *
                       MOVE    SPA-Z01-TYUKOEDYMD(IDX2)
                                                   TO  TNSP-YUKOEDYMD
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC           NOT =   ZERO
                           MOVE    "9004"          TO  SPA-ERRCD
                           DISPLAY "Z03 TENSUPLUS UPD ERR:" MCP-RC
                                   ",KEY:" TNSP-KEY
                       END-IF
                       ADD     1               TO  IDX1
                       ADD     1               TO  IDX2
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                       >   TBL-TNSP-YUKOSTYMD(IDX1)) AND
                           (TBL-TNSP-YUKOSTYMD(IDX1)
                                       NOT =   SPACE  )
      *                付加情報のみの場合、削除
                       MOVE    TBL-TENSUPLUS-REC (IDX1)
                                                   TO  TENSUPLUS-REC
                       MOVE    TNSP-SRYCD          TO  TNSP-UP-SRYCD
                       MOVE    TNSP-YUKOSTYMD      TO  TNSP-UP-YUKOSTYMD
                       MOVE    TNSP-YUKOEDYMD      TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       ADD     1               TO  IDX1
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                   NOT =   SPACE     )  AND
                           (SPA-Z01-TYUKOSTYMD(IDX2)
                                       <   TBL-TNSP-YUKOSTYMD(IDX1))
                       ADD     1               TO  IDX2
                   WHEN    OTHER
                       IF      (SPA-Z01-TYUKOSTYMD(IDX2)
                                           NOT =   SPACE   )
                           ADD     1               TO  IDX2
                       ELSE
                           IF    (TBL-TNSP-YUKOSTYMD(IDX1)
                                                 =   SPACE  )
                           ADD     1               TO  IDX1
                           END-IF
                       END-IF
               END-EVALUATE
      *
               END-IF
           END-PERFORM
      *
           .
       100-TENSUPLUS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  点数マスタ付加情報更新処理
      *****************************************************************
       200-TENSUPLUS-UPD-SEC            SECTION.
      *
      *    点数マスタ付加情報検索
           IF      SPA-Z01-SELNUM      >   ZERO
               MOVE    SPACE               TO  TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSP-YUKOEDYMD
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSUPLUS
               END-IF
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBLOSEC-SEC
           ELSE
      *        新規
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
      *
      *    新規で付加情報が登録されていないときは作成しない
      *    チェック
           IF    ( LNK-TNSP-SHONAME         =   SPACE  ) AND
                 ( LNK-TNSP-SAIKETUKBN      =   ZERO  ) AND
                 ( LNK-TNSP-ZENKNSKBN       =   ZERO  ) AND
                 ( LNK-TNSP-INPCHKKBN       =   ZERO  ) AND
                 ( LNK-TNSP-TENPUYAKKBN     =   SPACE ) AND
                 ( LNK-TNSP-ALLSURYOKBN     =   SPACE ) AND
                 ( LNK-TNSP-KANZANTANICD    =   SPACE ) AND
                 ( LNK-TNSP-KANZANTANINAME  =   SPACE ) AND
                 ( LNK-TNSP-KANZANCHI       =   ZERO  )
      *        上限回数
             AND ( LNK-TNSP-SANTEIRRKKBN    =   ZERO  )
             AND ( LNK-TNSP-JGNCNT          =   ZERO  )
             AND ( LNK-TNSP-JGNCNT1D        =   ZERO  )
             AND ( LNK-TNSP-JGNCNT1W        =   ZERO  )
             AND ( LNK-TNSP-JGNCNTERR       =   ZERO  )
      *        一般名記載
             AND ( LNK-TNSP-SOURYOHENKBN    =   ZERO  )
             AND ( LNK-TNSP-IPN-KISAIKBN    =   ZERO  )
             AND ( LNK-TNSP-IPN-KANZANTANICD    =   SPACE )
             AND ( LNK-TNSP-IPN-KANZANCHI   =   ZERO  )
               IF      FLG-TENSUPLUS    =  1
                   GO  TO  200-TENSUPLUS-UPD-EXT
               END-IF
           END-IF
      *
      *    有効期限更新
           IF      FLG-TENSUPLUS       =   ZERO
               EVALUATE    SPA-Z01-SYORIKBN
                   WHEN    3
      *                改正時処理
                       MOVE    ZS02-YUKOEDYMD      TO  TNSP-YUKOEDYMD
      *
                       MOVE    SPA-Z01-SRYCD       TO  TNSP-UP-SRYCD
                       MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOSTYMD
                       MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
      *
                       MOVE    SPA-OPID            TO  TNSP-OPID
                       MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
                       MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC           NOT =   ZERO
                           MOVE    "9004"          TO  SPA-ERRCD
                           DISPLAY "Z02 TENSUPLUS UPD ERR:" MCP-RC
                                   ",KEY:" TNSP-KEY
                           GO      TO      200-TENSUPLUS-UPD-EXT
                       END-IF
      *
                   WHEN    1
                   WHEN    2
      *                更新時処理
                       MOVE    ZS02-NAI-YUKOSTYMD   TO  TNSP-YUKOSTYMD
                       MOVE    ZS02-NAI-YUKOEDYMD   TO  TNSP-YUKOEDYMD
                       PERFORM 2001-TENSUPLUS-HEN-SEC
      *
                       MOVE    SPA-Z01-SRYCD       TO  TNSP-UP-SRYCD
                       MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOSTYMD
                       MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
      *
                       MOVE    SPA-OPID            TO  TNSP-OPID
                       MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
                       MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
                       MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                           MOVE    "9004"          TO  SPA-ERRCD
                           DISPLAY "Z02 TENSUPLUS UPD ERR:" MCP-RC
                                   ",KEY:" TNSP-KEY
                           GO      TO      200-TENSUPLUS-UPD-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    新規追加
           IF     (FLG-TENSUPLUS       =   1   )  OR 
                  (SPA-Z01-SYORIKBN    =   3    )
               INITIALIZE                      TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    ZS02-NAI-YUKOSTYMD  TO  TNSP-YUKOSTYMD
               MOVE    ZS02-NAI-YUKOEDYMD  TO  TNSP-YUKOEDYMD
               PERFORM 2001-TENSUPLUS-HEN-SEC
      *
               MOVE    SPA-OPID            TO  TNSP-OPID
               MOVE    SMCNDATE-YMD        TO  TNSP-CREYMD
               MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   MOVE    "9004"          TO  SPA-ERRCD
                   DISPLAY "Z02 TENSUPLUS INS ERR:" MCP-RC
                          ",KEY:" TNSP-KEY
               END-IF
           END-IF
      *
           .
       200-TENSUPLUS-UPD-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報編集
      *****************************************************************
       2001-TENSUPLUS-HEN-SEC          SECTION.
      *
           MOVE    LNK-TNSP-INPCHKKBN      TO  TNSP-INPCHKKBN
      *    採血料区分
           MOVE    LNK-TNSP-SAIKETUKBN     TO  TNSP-SAIKETUKBN
      *    前回検査日区分
           MOVE    LNK-TNSP-ZENKNSKBN      TO  TNSP-ZENKNSKBN
      *    処方名称
           MOVE    LNK-TNSP-SHONAME        TO  TNSP-SHONAME
           MOVE    LNK-TNSP-SHOYUKOKETA    TO  TNSP-SHOYUKOKETA
      *    分類区分
           MOVE    LNK-TNSP-TENPUYAKKBN    TO  TNSP-TENPUYAKKBN
      *    全数量記載区分
           MOVE    LNK-TNSP-ALLSURYOKBN    TO  TNSP-ALLSURYOKBN
      *    数量換算単位コード
           MOVE    LNK-TNSP-KANZANTANICD   TO  TNSP-KANZANTANICD
      *    数量換算単位名称
           MOVE    LNK-TNSP-KANZANTANINAME TO  TNSP-KANZANTANINAME
      *    数量換算値
           MOVE    LNK-TNSP-KANZANCHI      TO  TNSP-KANZANCHI
      *    算定履歴区分
           MOVE    LNK-TNSP-SANTEIRRKKBN   TO  TNSP-SANTEIRRKKBN
      *    月上限回数
           MOVE    LNK-TNSP-JGNCNT         TO  TNSP-JGNCNT
      *    日上限回数
           MOVE    LNK-TNSP-JGNCNT1D       TO  TNSP-JGNCNT1D
      *    週上限回数
           MOVE    LNK-TNSP-JGNCNT1W       TO  TNSP-JGNCNT1W
      *    エラー処理
           MOVE    LNK-TNSP-JGNCNTERR      TO  TNSP-JGNCNTERR
      *    総量編集区分
           MOVE    LNK-TNSP-SOURYOHENKBN   TO  TNSP-SOURYOHENKBN
      *    一般名記載区分
           MOVE    LNK-TNSP-IPN-KISAIKBN   TO  TNSP-IPN-KISAIKBN
      *    一般名記載単位
           MOVE    LNK-TNSP-IPN-KANZANTANICD
                                           TO  TNSP-IPN-KANZANTANICD
      *    一般名記載単位名
           MOVE    LNK-TNSP-IPN-KANZANTANINAME
                                           TO  TNSP-IPN-KANZANTANINAME
      *    一般名記載換算値
           MOVE    LNK-TNSP-IPN-KANZANCHI  TO  TNSP-IPN-KANZANCHI
      *    他月数
           MOVE    LNK-TNSP-JGNCNTETCM     TO  TNSP-JGNCNTETCM
      *    他回数
           MOVE    LNK-TNSP-JGNCNTETC      TO  TNSP-JGNCNTETC
      *
           .
       2001-TENSUPLUS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       990-TENSUPLUS-READ-EXT.
           EXIT.
      *
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
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *
