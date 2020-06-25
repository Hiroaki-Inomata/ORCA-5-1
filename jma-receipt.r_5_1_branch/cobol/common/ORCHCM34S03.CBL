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
       PROGRAM-ID.             ORCHCM34S03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 労災入院診療コードチェックサブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/03/29    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-TENSU               PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
      *    労災入院診療コードチェックサブ
           COPY    "CPORCHCM34S03.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
                                       HCM34S03-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
      *
           MOVE    ZERO            TO  HCM34S03-IGNORE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( HCM34S03-SRYYM     >=       "201004" )
               PERFORM 900-TENSU-KEY31-SEL-SEC
               IF    ( FLG-TENSU       =       ZERO )
                   MOVE    1           TO      HCM34S03-IGNORE
               END-IF
           END-IF
      *
           IF    ( HCM34S03-SRYCD      =       "190080470"
                                        OR     "190799670"
                                        OR     "190080570"
                                        OR     "190799770"
                                        OR     "190111690"
                                        OR     "190809090"
                                        OR     "190111790"
                                        OR     "190809190"
                                        OR     "190109170"
                                        OR     "190807570"
                                        OR     "190109270"
                                        OR     "190808670"
                                        OR     "190135570"
                                        OR     "190131570"
                                        OR     "190118570"
                                        OR     "190134870"
                                        OR     "190134970"
                                        OR     "190143070"
                                        OR     "190143170"
                                        OR     "190143970"
                                        OR     "190144070"
                                        OR     "190144570"
                                        OR     "190144670"
                                        OR     "190182590"
                                        OR     "190182690"
      *
                                        OR     "190183070"
                                        OR     "190183170"
                                        OR     "190183270"
                                        OR     "190188870"
                                        OR     "190188970"
                                        OR     "190189070"
                                        OR     "190189170"
                                        OR     "190189270"
                                        OR     "190189370"
                                        OR     "190190270"
                                        OR     "190190370"
                                        OR     "190212670"
                                        OR     "101900160"
                                                           )
            OR  (( HCM34S03-SRYCD      =       "190116990"
                                        OR     "190813390" )
             AND ( HCM34S03-SRYYM      <       "200604"    ))
            OR  (( HCM34S03-SRYCD      =       "190117070"
                                        OR     "190109670" )
             AND ( HCM34S03-SRYYM     >=       "200604" ))
            OR  (( HCM34S03-SRYCD      =       "190121970"
                                        OR     "190122670" )
             AND ( HCM34S03-SRYYM     >=       "200607"    ))
            OR  (( HCM34S03-SRYCD      =       "190126470"
                                        OR     "190117070"
                                        OR     "190126570"
                                        OR     "190109670"
                                        OR     "190126670"
                                        OR     "190126770"
                                        OR     "190119210"
                                        OR     "190121970"
                                        OR     "190122670" )
             AND ( HCM34S03-SRYYM     >=      "200804"    ))
            OR  (( HCM34S03-SRYCD      =      "190129870"  )
             AND ( HCM34S03-SRYCD     >=      "200804"
             AND   HCM34S03-SRYCD     <=      "201003"    ))
      *
               MOVE    1               TO     HCM34S03-IGNORE
      *
           END-IF
      *
           IF    (  HCM34S03-SRYCD     =      "190134270"
                                        OR    "190135670"
                                        OR    "190135470"
                                        OR    "190142970"
                                        OR    "190143370"
                                        OR    "190142870"
                                        OR    "190143270"
                                        OR    "190143470"
                                        OR    "190144770"
                                        OR    "190144870"
                                        OR    "190144970"
                                        OR    "190145070"
                                        OR    "190145170"
                                        OR    "190145270"
                                        OR    "190135070"
      *
                                        OR    "190161770"
                                        OR    "190162170"
                                        OR    "190161470"
                                        OR    "190161570"
                                        OR    "190161670"
                                        OR    "190162070"
                                        OR    "190161970"
                                        OR    "190161870"
      *
                                        OR    "190166070"
                                        OR    "190169070"
                                        OR    "190169170"
      *
                                        OR    "190198570"
                                        OR    "190198670"
                                        OR    "190201770"
                                        OR    "190201870"
                                        OR    "190205570"
                                        OR    "190205670"
                                        OR    "190205770"
                                        OR    "190205970"
                                        OR    "190206070"
                                                         )
               MOVE    ZERO             TO     HCM34S03-IGNORE
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY31-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    HCM34S03-SRYCD  TO  TNS-SRYCD
           MOVE    HCM34S03-SRYYM  TO  TNS-YUKOSTYMD
           MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key31"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key31"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY31-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
           .
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
           .
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
