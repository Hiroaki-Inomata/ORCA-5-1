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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSGAIFTN.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCSGAIFTN  /  負担金額算定サブ               
      *                                （外来用）                     
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/06/30    NACL−門脇    新規作成
      ****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04/10/01  自賠責特定器材等消費税対応
      *  01.00.02    NACL-門脇    04/11/25  福島県乳幼児特殊計算対応
      *  01.00.03    NACL-門脇    05/07/19  収納項目追加に伴う修正
      *  01.00.04    NACL-門脇    06/03/22  自立支援法対応
      *  03.05.00    NACL-門脇    07/05/08  グループ診療対応
      *  04.01.01    NACL-門脇    07/12/07  公害対応
      *
      *  04.06.01    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *
      *  04.08.01    NACL-門脇    16/02/12  第三者行為対応
      *  04.08.02    NACL-門脇    17/07/14  和歌山県地方公費対応
      *  04.08.03    NACL-門脇    18/04/26  現物給付メッセージフラグクリア
      *  04.08.04    NACL-門脇    18/05/15  同一会計分のデータ編集修正
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
      *    添え字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(02).
           03  IDX2                        PIC 9(03).
           03  IDX3                        PIC 9(03).
           03  IDY                         PIC 9(02).
           03  IDZ                         PIC 9(02).
           03  IDZ2                        PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK                      PIC 9(01).
           03  FLG-TOKUSYU-SAI             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-KENSU                   PIC 9(03).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-TAIHI-HKNCOMBINUM       PIC X(04).
           03  WRK-SYUTEN                  PIC 9(07).
           03  WRK-SINDAN                  PIC 9(07).
           03  WRK-MEISAI                  PIC 9(07).
           03  WRK-KYUKYU                  PIC 9(07).
           03  WRK-RYOYO                   PIC 9(07).
           03  WRK-SENINFLG                PIC X(01).
           03  WRK-ZAITAKU-KBN             PIC X(01).
           03  WRK-SONOTA-TBL.
              04  WRK-SONOTA-OCC           OCCURS  20.
                 05  WRK-SONOTA-SRYCD      PIC  X(09).
                 05  WRK-SONOTA-ZAIKAISU   PIC  9(07).
                 05  WRK-SONOTA-TANKA      PIC  9(07).
      *
           03  WRK-SKYMONEY-TBL.
              04  WRK-SKYMONEY-TBL-OCC           OCCURS  15.
                 05  WRK-SKYMONEY                PIC 9(07).
                 05  WRK-SYUHKNFTNMONEY          PIC 9(07).
                 05  WRK-SYUCOMPFTN              PIC 9(07).
                 05  WRK-SYUCOMPFTN-ENTANI       PIC 9(07)V9(01).
                 05  WRK-SYUCOMPTOTAL            PIC 9(07).
                 05  WRK-KOH1HKNFTNMONEY         PIC 9(07).
                 05  WRK-KOH1COMPFTN             PIC 9(07).
                 05  WRK-KOH1COMPFTN-ENTANI      PIC 9(07)V9(01).
                 05  WRK-KOH1COMPTOTAL           PIC 9(07).
                 05  WRK-KOH2HKNFTNMONEY         PIC 9(07).
                 05  WRK-KOH2COMPFTN             PIC 9(07).
                 05  WRK-KOH2COMPFTN-ENTANI      PIC 9(07)V9(01).
                 05  WRK-KOH2COMPTOTAL           PIC 9(07).
                 05  WRK-KOH3HKNFTNMONEY         PIC 9(07).
                 05  WRK-KOH3COMPFTN             PIC 9(07).
                 05  WRK-KOH3COMPFTN-ENTANI      PIC 9(07)V9(01).
                 05  WRK-KOH3COMPTOTAL           PIC 9(07).
                 05  WRK-KOH4HKNFTNMONEY         PIC 9(07).
                 05  WRK-KOH4COMPFTN             PIC 9(07).
                 05  WRK-KOH4COMPFTN-ENTANI      PIC 9(07)V9(01).
                 05  WRK-KOH4COMPTOTAL           PIC 9(07).
                 05  WRK-TOTAL-TGMONEY-TAX       PIC 9(07).
                 05  WRK-TGMONEY-TAX-SAI         PIC 9(07).
                 05  WRK-JIHI-TAX                PIC 9(07).
                 05  WRK-RJN-FTN                 PIC 9(07).
                 05  WRK-KOH-FTN                 PIC 9(07).
                 05  WRK-KOH-FTN-ENTANI          PIC 9(07)V9(01).
                 05  WRK-HKNTEKMONEY             PIC 9(07).
                 05  WRK-RSISHOSHIN-MONEY        PIC 9(07).
                 05  WRK-RSISAISHIN-MONEY        PIC 9(07).
                 05  WRK-RSISDO-MONEY            PIC 9(07).
                 05  WRK-RSIETC-MONEY            PIC 9(07).
                 05  WRK-RSI-TAX-SAI             PIC 9(07).
                 05  WRK-RSI-TOTAL               PIC 9(07).
                 05  WRK-RSIJIBAI-SKYMONEY       PIC 9(07).
                 05  WRK-SKYMONEY-TAX-SAI        PIC 9(07).
                 05  WRK-TOKUSYU-FTNMONEY        PIC 9(07).
                 05  WRK-TOKUSYU-COMPFTN         PIC 9(07).
                 05  WRK-TOKUSYU-ENTANI          PIC 9(07)V9(01).
                 05  WRK-DISCOUNT-MONEY          PIC 9(07).
      *
                 05  WRK-GRP-SGKMONEY            PIC S9(07).
                 05  WRK-GRP-SYUHKNFTNMONEY      PIC S9(07).
                 05  WRK-GRP-SYUCOMPFTN          PIC S9(07).
                 05  WRK-GRP-SYUCOMPFTN-ENTANI   PIC S9(07)V9(01).
                 05  WRK-GRP-SYUCOMPTOTAL        PIC S9(07).
                 05  WRK-GRP-KOH1HKNFTNMONEY     PIC S9(07).
                 05  WRK-GRP-KOH1COMPFTN         PIC S9(07).
                 05  WRK-GRP-KOH1COMPFTN-ENTANI  PIC S9(07)V9(01).
                 05  WRK-GRP-KOH1COMPTOTAL       PIC S9(07).
                 05  WRK-GRP-KOH2HKNFTNMONEY     PIC S9(07).
                 05  WRK-GRP-KOH2COMPFTN         PIC S9(07).
                 05  WRK-GRP-KOH2COMPFTN-ENTANI  PIC S9(07)V9(01).
                 05  WRK-GRP-KOH2COMPTOTAL       PIC S9(07).
                 05  WRK-GRP-KOH3HKNFTNMONEY     PIC S9(07).
                 05  WRK-GRP-KOH3COMPFTN         PIC S9(07).
                 05  WRK-GRP-KOH3COMPFTN-ENTANI  PIC S9(07)V9(01).
                 05  WRK-GRP-KOH3COMPTOTAL       PIC S9(07).
                 05  WRK-GRP-KOH4HKNFTNMONEY     PIC S9(07).
                 05  WRK-GRP-KOH4COMPFTN         PIC S9(07).
                 05  WRK-GRP-KOH4COMPFTN-ENTANI  PIC S9(07)V9(01).
                 05  WRK-GRP-KOH4COMPTOTAL       PIC S9(07).
                 05  WRK-GRP-TOTAL-TGMONEY-TAX   PIC S9(07).
                 05  WRK-GRP-TGMONEY-TAX-SAI     PIC S9(07).
                 05  WRK-GRP-JIHI-TAX            PIC S9(07).
                 05  WRK-GRP-RJN-FTN             PIC S9(07).
                 05  WRK-GRP-KOH-FTN             PIC S9(07).
                 05  WRK-GRP-KOH-FTN-ENTANI      PIC S9(07)V9(01).
                 05  WRK-GRP-HKNTEKMONEY         PIC S9(07).
                 05  WRK-GRP-RSISHOSHIN-MONEY    PIC S9(07).
                 05  WRK-GRP-RSISAISHIN-MONEY    PIC S9(07).
                 05  WRK-GRP-RSISDO-MONEY        PIC S9(07).
                 05  WRK-GRP-RSIETC-MONEY        PIC S9(07).
                 05  WRK-GRP-RSI-TAX-SAI         PIC S9(07).
                 05  WRK-GRP-RSI-TOTAL           PIC S9(07).
                 05  WRK-GRP-RSIJIBAI-SKYMONEY   PIC S9(07).
                 05  WRK-GRP-SKYMONEY-TAX-SAI    PIC S9(07).
                 05  WRK-GRP-TOKUSYU-FTNMONEY    PIC S9(07).
                 05  WRK-GRP-TOKUSYU-COMPFTN     PIC S9(07).
                 05  WRK-GRP-TOKUSYU-ENTANI      PIC S9(07)V9(01).
                 05  WRK-GRP-DISCOUNT-MONEY      PIC S9(07).
      *
           03  WRK-RUI.
              04  WRK-RUI-SKYMONEY               PIC 9(07).
              04  WRK-RUI-SYUHKNFTNMONEY         PIC 9(07).
              04  WRK-RUI-SYUCOMPFTN             PIC 9(07).
              04  WRK-RUI-SYUCOMPFTN-ENTANI      PIC 9(07)V9(01).
              04  WRK-RUI-SYUCOMPTOTAL           PIC 9(07).
              04  WRK-RUI-KOH1HKNFTNMONEY        PIC 9(07).
              04  WRK-RUI-KOH1COMPFTN            PIC 9(07).
              04  WRK-RUI-KOH1COMPFTN-ENTANI     PIC 9(07)V9(01).
              04  WRK-RUI-KOH1COMPTOTAL          PIC 9(07).
              04  WRK-RUI-KOH2HKNFTNMONEY        PIC 9(07).
              04  WRK-RUI-KOH2COMPFTN            PIC 9(07).
              04  WRK-RUI-KOH2COMPFTN-ENTANI     PIC 9(07)V9(01).
              04  WRK-RUI-KOH2COMPTOTAL          PIC 9(07).
              04  WRK-RUI-KOH3HKNFTNMONEY        PIC 9(07).
              04  WRK-RUI-KOH3COMPFTN            PIC 9(07).
              04  WRK-RUI-KOH3COMPFTN-ENTANI     PIC 9(07)V9(01).
              04  WRK-RUI-KOH3COMPTOTAL          PIC 9(07).
              04  WRK-RUI-KOH4HKNFTNMONEY        PIC 9(07).
              04  WRK-RUI-KOH4COMPFTN            PIC 9(07).
              04  WRK-RUI-KOH4COMPFTN-ENTANI     PIC 9(07)V9(01).
              04  WRK-RUI-KOH4COMPTOTAL          PIC 9(07).
              04  WRK-RUI-TOTAL-TGMONEY-TAX      PIC 9(07).
              04  WRK-RUI-TGMONEY-TAX-SAI        PIC 9(07).
              04  WRK-RUI-JIHI-TAX               PIC 9(07).
              04  WRK-RUI-RJN-FTN                PIC 9(07).
              04  WRK-RUI-KOH-FTN                PIC 9(07).
              04  WRK-RUI-KOH-FTN-ENTANI         PIC 9(07)V9(01).
              04  WRK-RUI-HKNTEKMONEY            PIC 9(07).
              04  WRK-RUI-RSISHOSHIN-MONEY       PIC 9(07).
              04  WRK-RUI-RSISAISHIN-MONEY       PIC 9(07).
              04  WRK-RUI-RSISDO-MONEY           PIC 9(07).
              04  WRK-RUI-RSIETC-MONEY           PIC 9(07).
              04  WRK-RUI-RSI-TAX-SAI            PIC 9(07).
              04  WRK-RUI-RSI-TOTAL              PIC 9(07).
              04  WRK-RUI-RSIJIBAI-SKYMONEY      PIC 9(07).
              04  WRK-RUI-SKYMONEY-TAX-SAI       PIC 9(07).
              04  WRK-RUI-TOKUSYU-FTNMONEY       PIC 9(07).
              04  WRK-RUI-TOKUSYU-COMPFTN        PIC 9(07).
              04  WRK-RUI-TOKUSYU-ENTANI         PIC 9(07)V9(01).
              04  WRK-RUI-DISCOUNT-MONEY         PIC 9(07).
      *
           03  WRK-KEISAN                  PIC S9(07).
           03  WRK-KEISAN2                 PIC S9(07)V9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY               PIC 9(04).
               05  WRK-SYSMM               PIC 9(02).
               05  WRK-SYSDD               PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ワーク収納マスター
       01  WRK-SYUNOU-REC-T.
         02 WRK-SYUNOU-REC     OCCURS 15.
           COPY    "CPSYUNOU.INC" REPLACING  //SYU-// BY //WRK-SYU-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCS01.INC"   REPLACING  //LNK-// BY //WRK-//.
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "MCPAREA". 
           COPY  "CPORCS01.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 15.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
      *
grpsys     COPY    "COMMON-SPA".
      /
       PROCEDURE           DIVISION  USING
                                     MCPAREA
                                     LNK-ORCS01-REC
                                     LNK-SYUNOU-REC-T
                                     SPA-AREA
           .
      ******************************************************************
      *        依頼制御処理
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
           MOVE       ZERO      TO     LNK-S01-RC.
      *
      *    合計レコードをもつレコードのフラグ等、引数のセット
      *
           INITIALIZE                  FLG-AREA
                                       CNT-AREA.
      *
           MOVE       SPACE     TO     WRK-TAIHI-HKNCOMBINUM.
           MOVE       ZERO      TO     WRK-SYUTEN.
           MOVE       ZERO      TO     WRK-SINDAN.
           MOVE       ZERO      TO     WRK-MEISAI.
           MOVE       ZERO      TO     WRK-RYOYO.
           MOVE       SPACE     TO     WRK-SENINFLG.
           INITIALIZE                  WRK-SONOTA-TBL.
           MOVE       ZERO      TO     WRK-KYUKYU.
           MOVE       ZERO      TO     IDX3.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX                 >    15 )
                                OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                IF   LNK-SYU-SRYKA(IDX)  =  "00"
      *
                    PERFORM    VARYING   IDY    FROM   1   BY   1
                                UNTIL    IDY    >    IDX
      *
                      IF   LNK-SYU-HKNCOMBINUM(IDY)
                                         =  LNK-SYU-HKNCOMBINUM(IDX)
                          MOVE   "1"   TO   LNK-S01-TTLARIFLG(IDY)
                      END-IF
      *
                    END-PERFORM
      *
                    IF     LNK-SYU-SYUHKNNUM(IDX)  =   "973"
                       MOVE   WRK-SYUTEN    TO    LNK-S01-SYUTEN(IDX)
                       MOVE   WRK-SINDAN    TO    LNK-S01-SINDAN(IDX)
                       MOVE   WRK-MEISAI    TO    LNK-S01-MEISAI(IDX)
                       MOVE   WRK-RYOYO     TO    LNK-S01-RYOYO(IDX)
                       MOVE   WRK-SONOTA-TBL  TO  LNK-S01-SONOTA(IDX)
                       MOVE   WRK-KYUKYU    TO    LNK-S01-KYUKYU(IDX)
                    END-IF
                    IF     LNK-SYU-KOH1HKNNUM(IDX) =   "970"
                       MOVE   WRK-SONOTA-TBL  TO  LNK-S01-SONOTA(IDX)
                    END-IF
                    IF     LNK-SYU-SYUHKNNUM(IDX)  =   "975"
                       MOVE   WRK-SYUTEN    TO    LNK-S01-SYUTEN(IDX)
                    END-IF
      *
                    MOVE      WRK-SENINFLG  TO    LNK-S01-SENINFLG(IDX)
                ELSE
                    IF   WRK-TAIHI-HKNCOMBINUM  NOT =  SPACE
                    AND  WRK-TAIHI-HKNCOMBINUM
                                        NOT =   LNK-SYU-HKNCOMBINUM(IDX)
                          MOVE    ZERO    TO   WRK-SYUTEN
                          MOVE    ZERO    TO   WRK-SINDAN
                          MOVE    ZERO    TO   WRK-MEISAI
                          MOVE    ZERO    TO   WRK-RYOYO
                          MOVE    SPACE   TO   WRK-SENINFLG
                          INITIALIZE           WRK-SONOTA-TBL
                          MOVE    ZERO    TO   WRK-KYUKYU
                          MOVE    ZERO    TO   IDX3
                    END-IF
      *
                    IF  LNK-SYU-SYUHKNNUM(IDX)  =   "973"
                      COMPUTE   WRK-SYUTEN  =  WRK-SYUTEN
                                            +  LNK-S01-SYUTEN(IDX)
                      COMPUTE   WRK-SINDAN  =  WRK-SINDAN
                                            +  LNK-S01-SINDAN(IDX)
                      COMPUTE   WRK-MEISAI  =  WRK-MEISAI
                                            +  LNK-S01-MEISAI(IDX)
                      COMPUTE   WRK-RYOYO   =  WRK-RYOYO
                                            +  LNK-S01-RYOYO(IDX)
                      PERFORM    VARYING   IDX2    FROM   1   BY   1
                        UNTIL  (IDX2                           >    20 )
                          OR   (LNK-S01-SONOTA-SRYCD(IDX IDX2) =  SPACE)
                         IF   IDX3  >= 20
                            DISPLAY "TABLE OVER"
                         ELSE
                            ADD    1      TO   IDX3
                            MOVE   LNK-S01-SONOTA-SRYCD(IDX IDX2)
                                          TO   WRK-SONOTA-SRYCD(IDX3)
                            MOVE   LNK-S01-SONOTA-ZAIKAISU(IDX IDX2)
                                          TO   WRK-SONOTA-ZAIKAISU(IDX3)
                            MOVE   LNK-S01-SONOTA-TANKA(IDX IDX2)
                                          TO   WRK-SONOTA-TANKA(IDX3)
                         END-IF
                      END-PERFORM
                      COMPUTE   WRK-KYUKYU  =  WRK-KYUKYU
                                            +  LNK-S01-KYUKYU(IDX)
                    END-IF
      *
                    IF  LNK-SYU-KOH1HKNNUM(IDX) =   "970"
                      PERFORM    VARYING   IDX2    FROM   1   BY   1
                        UNTIL  (IDX2                           >    20 )
                          OR   (LNK-S01-SONOTA-SRYCD(IDX IDX2) =  SPACE)
                         IF   IDX3  >= 20
                            DISPLAY "TABLE OVER"
                         ELSE
                            ADD    1      TO   IDX3
                            MOVE   LNK-S01-SONOTA-SRYCD(IDX IDX2)
                                          TO   WRK-SONOTA-SRYCD(IDX3)
                            MOVE   LNK-S01-SONOTA-ZAIKAISU(IDX IDX2)
                                          TO   WRK-SONOTA-ZAIKAISU(IDX3)
                            MOVE   LNK-S01-SONOTA-TANKA(IDX IDX2)
                                          TO   WRK-SONOTA-TANKA(IDX3)
                         END-IF
                      END-PERFORM
                    END-IF
      *
                    IF  LNK-SYU-SYUHKNNUM(IDX)  =   "975"
                      COMPUTE   WRK-SYUTEN  =  WRK-SYUTEN
                                            +  LNK-S01-SYUTEN(IDX)
                    END-IF
      *
                    IF      WRK-SENINFLG            =  SPACE
                    AND     LNK-S01-SENINFLG(IDX)   =   "1"
                        MOVE     "1"       TO  WRK-SENINFLG
                    END-IF
      *
                    MOVE    LNK-SYU-HKNCOMBINUM(IDX)
                                           TO  WRK-TAIHI-HKNCOMBINUM
                END-IF
      *
           END-PERFORM.
      *
      *    負担金計算
           MOVE     LNK-S01-KOUREI-ZAITAKU-KBN
                                           TO  WRK-ZAITAKU-KBN.
      *
           PERFORM     100-SANTEI-SEC.
      *
      *    特殊計算（再計算）
           IF  (CNT-KENSU        >    1)
           AND (FLG-TOKUSYU-SAI  =    1)
             MOVE   "1"   TO   LNK-S01-RETRY-FLG
             PERFORM   100-SANTEI-SEC
           END-IF.
      *
      *    収納の項目再編集の為の差額算出
      *
           INITIALIZE                  WRK-SKYMONEY-TBL.
           INITIALIZE                  WRK-RUI.
           MOVE       SPACE     TO     WRK-TAIHI-HKNCOMBINUM.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX                 >    15 )
                                OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                                OR   (LNK-S01-RC      NOT =  ZERO )
      *
             IF   LNK-SYU-SRYKA(IDX)  NOT =  "00"
      *
                 IF   WRK-TAIHI-HKNCOMBINUM  NOT =  SPACE
                 AND  WRK-TAIHI-HKNCOMBINUM
                                     NOT =   LNK-SYU-HKNCOMBINUM(IDX)
                       INITIALIZE                  WRK-RUI
                 END-IF
      *
                 MOVE    LNK-SYU-SKYMONEY(IDX)   TO   WRK-SKYMONEY(IDX)
                 ADD     LNK-SYU-SKYMONEY(IDX)   TO   WRK-RUI-SKYMONEY
      *
                 IF      LNK-SYU-SYUHKNNUM(IDX)  NOT =  SPACE
                    MOVE    LNK-SYU-SYUHKNFTNMONEY(IDX)
                                       TO   WRK-SYUHKNFTNMONEY(IDX)
                    ADD     LNK-SYU-SYUHKNFTNMONEY(IDX)
                                       TO   WRK-RUI-SYUHKNFTNMONEY
                    MOVE    LNK-SYU-SYUCOMPFTN(IDX)
                                       TO   WRK-SYUCOMPFTN(IDX)
                    ADD     LNK-SYU-SYUCOMPFTN(IDX)
                                       TO   WRK-RUI-SYUCOMPFTN
                    MOVE    LNK-SYU-SYUCOMPFTN-ENTANI(IDX)
                                       TO   WRK-SYUCOMPFTN-ENTANI(IDX)
                    ADD     LNK-SYU-SYUCOMPFTN-ENTANI(IDX)
                                       TO   WRK-RUI-SYUCOMPFTN-ENTANI
                    MOVE    LNK-SYU-SYUCOMPTOTAL(IDX)
                                       TO   WRK-SYUCOMPTOTAL(IDX)
                    ADD     LNK-SYU-SYUCOMPTOTAL(IDX)
                                       TO   WRK-RUI-SYUCOMPTOTAL
                 END-IF
                 IF      LNK-SYU-KOH1HKNNUM(IDX)  NOT =  SPACE
                    MOVE    LNK-SYU-KOH1HKNFTNMONEY(IDX)
                                       TO   WRK-KOH1HKNFTNMONEY(IDX)
                    ADD     LNK-SYU-KOH1HKNFTNMONEY(IDX)
                                       TO   WRK-RUI-KOH1HKNFTNMONEY
                    MOVE    LNK-SYU-KOH1COMPFTN(IDX)
                                       TO   WRK-KOH1COMPFTN(IDX)
                    ADD     LNK-SYU-KOH1COMPFTN(IDX)
                                       TO   WRK-RUI-KOH1COMPFTN
                    MOVE    LNK-SYU-KOH1COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH1COMPFTN-ENTANI(IDX)
                    ADD     LNK-SYU-KOH1COMPFTN-ENTANI(IDX)
                                       TO   WRK-RUI-KOH1COMPFTN-ENTANI
                    MOVE    LNK-SYU-KOH1COMPTOTAL(IDX)
                                       TO   WRK-KOH1COMPTOTAL(IDX)
                    ADD     LNK-SYU-KOH1COMPTOTAL(IDX)
                                       TO   WRK-RUI-KOH1COMPTOTAL
                 END-IF
                 IF      LNK-SYU-KOH2HKNNUM(IDX)  NOT =  SPACE
                    MOVE    LNK-SYU-KOH2HKNFTNMONEY(IDX)
                                       TO   WRK-KOH2HKNFTNMONEY(IDX)
                    ADD     LNK-SYU-KOH2HKNFTNMONEY(IDX)
                                       TO   WRK-RUI-KOH2HKNFTNMONEY
                    MOVE    LNK-SYU-KOH2COMPFTN(IDX)
                                       TO   WRK-KOH2COMPFTN(IDX)
                    ADD     LNK-SYU-KOH2COMPFTN(IDX)
                                       TO   WRK-RUI-KOH2COMPFTN
                    MOVE    LNK-SYU-KOH2COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH2COMPFTN-ENTANI(IDX)
                    ADD     LNK-SYU-KOH2COMPFTN-ENTANI(IDX)
                                       TO   WRK-RUI-KOH2COMPFTN-ENTANI
                    MOVE    LNK-SYU-KOH2COMPTOTAL(IDX)
                                       TO   WRK-KOH2COMPTOTAL(IDX)
                    ADD     LNK-SYU-KOH2COMPTOTAL(IDX)
                                       TO   WRK-RUI-KOH2COMPTOTAL
                 END-IF
                 IF      LNK-SYU-KOH3HKNNUM(IDX)  NOT =  SPACE
                    MOVE    LNK-SYU-KOH3HKNFTNMONEY(IDX)
                                       TO   WRK-KOH3HKNFTNMONEY(IDX)
                    ADD     LNK-SYU-KOH3HKNFTNMONEY(IDX)
                                       TO   WRK-RUI-KOH3HKNFTNMONEY
                    MOVE    LNK-SYU-KOH3COMPFTN(IDX)
                                       TO   WRK-KOH3COMPFTN(IDX)
                    ADD     LNK-SYU-KOH3COMPFTN(IDX)
                                       TO   WRK-RUI-KOH3COMPFTN
                    MOVE    LNK-SYU-KOH3COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH3COMPFTN-ENTANI(IDX)
                    ADD     LNK-SYU-KOH3COMPFTN-ENTANI(IDX)
                                       TO   WRK-RUI-KOH3COMPFTN-ENTANI
                    MOVE    LNK-SYU-KOH3COMPTOTAL(IDX)
                                       TO   WRK-KOH3COMPTOTAL(IDX)
                    ADD     LNK-SYU-KOH3COMPTOTAL(IDX)
                                       TO   WRK-RUI-KOH3COMPTOTAL
                 END-IF
                 IF      LNK-SYU-KOH4HKNNUM(IDX)  NOT =  SPACE
                    MOVE    LNK-SYU-KOH4HKNFTNMONEY(IDX)
                                       TO   WRK-KOH4HKNFTNMONEY(IDX)
                    ADD     LNK-SYU-KOH4HKNFTNMONEY(IDX)
                                       TO   WRK-RUI-KOH4HKNFTNMONEY
                    MOVE    LNK-SYU-KOH4COMPFTN(IDX)
                                       TO   WRK-KOH4COMPFTN(IDX)
                    ADD     LNK-SYU-KOH4COMPFTN(IDX)
                                       TO   WRK-RUI-KOH4COMPFTN
                    MOVE    LNK-SYU-KOH4COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH4COMPFTN-ENTANI(IDX)
                    ADD     LNK-SYU-KOH4COMPFTN-ENTANI(IDX)
                                       TO   WRK-RUI-KOH4COMPFTN-ENTANI
                    MOVE    LNK-SYU-KOH4COMPTOTAL(IDX)
                                       TO   WRK-KOH4COMPTOTAL(IDX)
                    ADD     LNK-SYU-KOH4COMPTOTAL(IDX)
                                       TO   WRK-RUI-KOH4COMPTOTAL
                 END-IF
      *
                 MOVE    LNK-SYU-TOTAL-TGMONEY-TAX(IDX)
                                       TO   WRK-TOTAL-TGMONEY-TAX(IDX)
                 ADD     LNK-SYU-TOTAL-TGMONEY-TAX(IDX)
                                       TO   WRK-RUI-TOTAL-TGMONEY-TAX
      *
                 MOVE    LNK-SYU-TGMONEY-TAX-SAI(IDX)
                                       TO   WRK-TGMONEY-TAX-SAI(IDX)
                 ADD     LNK-SYU-TGMONEY-TAX-SAI(IDX)
                                       TO   WRK-RUI-TGMONEY-TAX-SAI
      *
                 MOVE    LNK-SYU-JIHI-TAX(IDX)
                                       TO   WRK-JIHI-TAX(IDX)
                 ADD     LNK-SYU-JIHI-TAX(IDX)
                                       TO   WRK-RUI-JIHI-TAX
      *
                 MOVE    LNK-SYU-RJN-FTN(IDX)
                                       TO   WRK-RJN-FTN(IDX)
                 ADD     LNK-SYU-RJN-FTN(IDX)
                                       TO   WRK-RUI-RJN-FTN
      *
                 MOVE    LNK-SYU-KOH-FTN(IDX)
                                       TO   WRK-KOH-FTN(IDX)
                 ADD     LNK-SYU-KOH-FTN(IDX)
                                       TO   WRK-RUI-KOH-FTN
      *
                 MOVE    LNK-SYU-KOH-FTN-ENTANI(IDX)
                                       TO   WRK-KOH-FTN-ENTANI(IDX)
                 ADD     LNK-SYU-KOH-FTN-ENTANI(IDX)
                                       TO   WRK-RUI-KOH-FTN-ENTANI
      *
                 MOVE    LNK-SYU-HKNTEKMONEY(IDX)
                                       TO   WRK-HKNTEKMONEY(IDX)
                 ADD     LNK-SYU-HKNTEKMONEY(IDX)
                                       TO   WRK-RUI-HKNTEKMONEY
      *
                 MOVE    LNK-SYU-RSISHOSHIN-MONEY(IDX)
                                       TO   WRK-RSISHOSHIN-MONEY(IDX)
                 ADD     LNK-SYU-RSISHOSHIN-MONEY(IDX)
                                       TO   WRK-RUI-RSISHOSHIN-MONEY
      *
                 MOVE    LNK-SYU-RSISAISHIN-MONEY(IDX)
                                       TO   WRK-RSISAISHIN-MONEY(IDX)
                 ADD     LNK-SYU-RSISAISHIN-MONEY(IDX)
                                       TO   WRK-RUI-RSISAISHIN-MONEY
      *
                 MOVE    LNK-SYU-RSISDO-MONEY(IDX)
                                       TO   WRK-RSISDO-MONEY(IDX)
                 ADD     LNK-SYU-RSISDO-MONEY(IDX)
                                       TO   WRK-RUI-RSISDO-MONEY
      *
                 MOVE    LNK-SYU-RSIETC-MONEY(IDX)
                                       TO   WRK-RSIETC-MONEY(IDX)
                 ADD     LNK-SYU-RSIETC-MONEY(IDX)
                                       TO   WRK-RUI-RSIETC-MONEY
      *
                 MOVE    LNK-SYU-RSI-TAX-SAI(IDX)
                                       TO   WRK-RSI-TAX-SAI(IDX)
                 ADD     LNK-SYU-RSI-TAX-SAI(IDX)
                                       TO   WRK-RUI-RSI-TAX-SAI
      *
                 MOVE    LNK-SYU-RSI-TOTAL(IDX)
                                       TO   WRK-RSI-TOTAL(IDX)
                 ADD     LNK-SYU-RSI-TOTAL(IDX)
                                       TO   WRK-RUI-RSI-TOTAL
      *
                 MOVE    LNK-SYU-RSIJIBAI-SKYMONEY(IDX)
                                       TO   WRK-RSIJIBAI-SKYMONEY(IDX)
                 ADD     LNK-SYU-RSIJIBAI-SKYMONEY(IDX)
                                       TO   WRK-RUI-RSIJIBAI-SKYMONEY
      *
                 MOVE    LNK-SYU-SKYMONEY-TAX-SAI(IDX)
                                       TO   WRK-SKYMONEY-TAX-SAI(IDX)
                 ADD     LNK-SYU-SKYMONEY-TAX-SAI(IDX)
                                       TO   WRK-RUI-SKYMONEY-TAX-SAI
      *
                 MOVE    LNK-SYU-JYO-HKNFTNMONEY(IDX)
                                       TO   WRK-TOKUSYU-FTNMONEY(IDX)
                 ADD     LNK-SYU-JYO-HKNFTNMONEY(IDX)
                                       TO   WRK-RUI-TOKUSYU-FTNMONEY
      *
                 MOVE    LNK-SYU-JYO-COMPFTN(IDX)
                                       TO   WRK-TOKUSYU-COMPFTN(IDX)
                 ADD     LNK-SYU-JYO-COMPFTN(IDX)
                                       TO   WRK-RUI-TOKUSYU-COMPFTN
      *
                 MOVE    LNK-SYU-JYO-COMPFTN-ENTANI(IDX)
                                       TO   WRK-TOKUSYU-ENTANI(IDX)
                 ADD     LNK-SYU-JYO-COMPFTN-ENTANI(IDX)
                                       TO   WRK-RUI-TOKUSYU-ENTANI
      *
                 MOVE    LNK-SYU-DISCOUNT-MONEY(IDX)
                                       TO   WRK-DISCOUNT-MONEY(IDX)
                 ADD     LNK-SYU-DISCOUNT-MONEY(IDX)
                                       TO   WRK-RUI-DISCOUNT-MONEY
      *
      *
                 MOVE    LNK-SYU-HKNCOMBINUM(IDX)
                                       TO    WRK-TAIHI-HKNCOMBINUM
      *
             ELSE
      *
                IF     LNK-S01-KAGRPFLG     =   "0"
      *
                   MOVE     LNK-SYU-SKYMONEY(IDX) TO   WRK-SKYMONEY(IDX)
                   COMPUTE  WRK-GRP-SGKMONEY(IDX)  =   WRK-SKYMONEY(IDX)
                                                   -   WRK-RUI-SKYMONEY
      *
                   IF       LNK-SYU-SYUHKNNUM(IDX)  NOT =  SPACE
                      MOVE     LNK-SYU-SYUHKNFTNMONEY(IDX)
                                       TO   WRK-SYUHKNFTNMONEY(IDX)
                      COMPUTE  WRK-GRP-SYUHKNFTNMONEY(IDX)  =
                               WRK-SYUHKNFTNMONEY(IDX)      -
                               WRK-RUI-SYUHKNFTNMONEY
                      MOVE     LNK-SYU-SYUCOMPFTN(IDX)
                                       TO   WRK-SYUCOMPFTN(IDX)
                      COMPUTE  WRK-GRP-SYUCOMPFTN(IDX)  =
                               WRK-SYUCOMPFTN(IDX)      -
                               WRK-RUI-SYUCOMPFTN
                      MOVE     LNK-SYU-SYUCOMPFTN-ENTANI(IDX)
                                       TO   WRK-SYUCOMPFTN-ENTANI(IDX)
                      COMPUTE  WRK-GRP-SYUCOMPFTN-ENTANI(IDX)  =
                               WRK-SYUCOMPFTN-ENTANI(IDX)      -
                               WRK-RUI-SYUCOMPFTN-ENTANI
                      MOVE     LNK-SYU-SYUCOMPTOTAL(IDX)
                                       TO   WRK-SYUCOMPTOTAL(IDX)
                      COMPUTE  WRK-GRP-SYUCOMPTOTAL(IDX)  =
                               WRK-SYUCOMPTOTAL(IDX)      -
                               WRK-RUI-SYUCOMPTOTAL
                   END-IF
                   IF       LNK-SYU-KOH1HKNNUM(IDX)  NOT =  SPACE
                      MOVE     LNK-SYU-KOH1HKNFTNMONEY(IDX)
                                       TO   WRK-KOH1HKNFTNMONEY(IDX)
                      COMPUTE  WRK-GRP-KOH1HKNFTNMONEY(IDX)  =
                               WRK-KOH1HKNFTNMONEY(IDX)      -
                               WRK-RUI-KOH1HKNFTNMONEY
                      MOVE     LNK-SYU-KOH1COMPFTN(IDX)
                                       TO   WRK-KOH1COMPFTN(IDX)
                      COMPUTE  WRK-GRP-KOH1COMPFTN(IDX)  =
                               WRK-KOH1COMPFTN(IDX)      -
                               WRK-RUI-KOH1COMPFTN
                      MOVE     LNK-SYU-KOH1COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH1COMPFTN-ENTANI(IDX)
                      COMPUTE  WRK-GRP-KOH1COMPFTN-ENTANI(IDX)  =
                               WRK-KOH1COMPFTN-ENTANI(IDX)      -
                               WRK-RUI-KOH1COMPFTN-ENTANI
                      MOVE     LNK-SYU-KOH1COMPTOTAL(IDX)
                                       TO   WRK-KOH1COMPTOTAL(IDX)
                      COMPUTE  WRK-GRP-KOH1COMPTOTAL(IDX)  =
                               WRK-KOH1COMPTOTAL(IDX)      -
                               WRK-RUI-KOH1COMPTOTAL
                   END-IF
                   IF       LNK-SYU-KOH2HKNNUM(IDX)  NOT =  SPACE
                      MOVE     LNK-SYU-KOH2HKNFTNMONEY(IDX)
                                       TO   WRK-KOH2HKNFTNMONEY(IDX)
                      COMPUTE  WRK-GRP-KOH2HKNFTNMONEY(IDX)  =
                               WRK-KOH2HKNFTNMONEY(IDX)      -
                               WRK-RUI-KOH2HKNFTNMONEY
                      MOVE     LNK-SYU-KOH2COMPFTN(IDX)
                                       TO   WRK-KOH2COMPFTN(IDX)
                      COMPUTE  WRK-GRP-KOH2COMPFTN(IDX)  =
                               WRK-KOH2COMPFTN(IDX)      -
                               WRK-RUI-KOH2COMPFTN
                      MOVE     LNK-SYU-KOH2COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH2COMPFTN-ENTANI(IDX)
                      COMPUTE  WRK-GRP-KOH2COMPFTN-ENTANI(IDX)  =
                               WRK-KOH2COMPFTN-ENTANI(IDX)      -
                               WRK-RUI-KOH2COMPFTN-ENTANI
                      MOVE     LNK-SYU-KOH2COMPTOTAL(IDX)
                                       TO   WRK-KOH2COMPTOTAL(IDX)
                      COMPUTE  WRK-GRP-KOH2COMPTOTAL(IDX)  =
                               WRK-KOH2COMPTOTAL(IDX)      -
                               WRK-RUI-KOH2COMPTOTAL
                   END-IF
                   IF       LNK-SYU-KOH3HKNNUM(IDX)  NOT =  SPACE
                      MOVE     LNK-SYU-KOH3HKNFTNMONEY(IDX)
                                       TO   WRK-KOH3HKNFTNMONEY(IDX)
                      COMPUTE  WRK-GRP-KOH3HKNFTNMONEY(IDX)  =
                               WRK-KOH3HKNFTNMONEY(IDX)      -
                               WRK-RUI-KOH3HKNFTNMONEY
                      MOVE     LNK-SYU-KOH3COMPFTN(IDX)
                                       TO   WRK-KOH3COMPFTN(IDX)
                      COMPUTE  WRK-GRP-KOH3COMPFTN(IDX)  =
                               WRK-KOH3COMPFTN(IDX)      -
                               WRK-RUI-KOH3COMPFTN
                      MOVE     LNK-SYU-KOH3COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH3COMPFTN-ENTANI(IDX)
                      COMPUTE  WRK-GRP-KOH3COMPFTN-ENTANI(IDX)  =
                               WRK-KOH3COMPFTN-ENTANI(IDX)      -
                               WRK-RUI-KOH3COMPFTN-ENTANI
                      MOVE     LNK-SYU-KOH3COMPTOTAL(IDX)
                                       TO   WRK-KOH3COMPTOTAL(IDX)
                      COMPUTE  WRK-GRP-KOH3COMPTOTAL(IDX)  =
                               WRK-KOH3COMPTOTAL(IDX)      -
                               WRK-RUI-KOH3COMPTOTAL
                   END-IF
                   IF       LNK-SYU-KOH4HKNNUM(IDX)  NOT =  SPACE
                      MOVE     LNK-SYU-KOH4HKNFTNMONEY(IDX)
                                       TO   WRK-KOH4HKNFTNMONEY(IDX)
                      COMPUTE  WRK-GRP-KOH4HKNFTNMONEY(IDX)  =
                               WRK-KOH4HKNFTNMONEY(IDX)      -
                               WRK-RUI-KOH4HKNFTNMONEY
                      MOVE     LNK-SYU-KOH4COMPFTN(IDX)
                                       TO   WRK-KOH4COMPFTN(IDX)
                      COMPUTE  WRK-GRP-KOH4COMPFTN(IDX)  =
                               WRK-KOH4COMPFTN(IDX)      -
                               WRK-RUI-KOH4COMPFTN
                      MOVE     LNK-SYU-KOH4COMPFTN-ENTANI(IDX)
                                       TO   WRK-KOH4COMPFTN-ENTANI(IDX)
                      COMPUTE  WRK-GRP-KOH4COMPFTN-ENTANI(IDX)  =
                               WRK-KOH4COMPFTN-ENTANI(IDX)      -
                               WRK-RUI-KOH4COMPFTN-ENTANI
                      MOVE     LNK-SYU-KOH4COMPTOTAL(IDX)
                                       TO   WRK-KOH4COMPTOTAL(IDX)
                      COMPUTE  WRK-GRP-KOH4COMPTOTAL(IDX)  =
                               WRK-KOH4COMPTOTAL(IDX)      -
                               WRK-RUI-KOH4COMPTOTAL
                   END-IF
      *
                   MOVE     LNK-SYU-TOTAL-TGMONEY-TAX(IDX)
                                       TO   WRK-TOTAL-TGMONEY-TAX(IDX)
                   COMPUTE  WRK-GRP-TOTAL-TGMONEY-TAX(IDX)  =
                            WRK-TOTAL-TGMONEY-TAX(IDX)      -
                            WRK-RUI-TOTAL-TGMONEY-TAX
      *
                   MOVE     LNK-SYU-TGMONEY-TAX-SAI(IDX)
                                       TO   WRK-TGMONEY-TAX-SAI(IDX)
                   COMPUTE  WRK-GRP-TGMONEY-TAX-SAI(IDX)  =
                            WRK-TGMONEY-TAX-SAI(IDX)      -
                            WRK-RUI-TGMONEY-TAX-SAI
      *
                   MOVE     LNK-SYU-JIHI-TAX(IDX)
                                       TO   WRK-JIHI-TAX(IDX)
                   COMPUTE  WRK-GRP-JIHI-TAX(IDX)  =
                            WRK-JIHI-TAX(IDX)      -
                            WRK-RUI-JIHI-TAX
      *
                   MOVE     LNK-SYU-RJN-FTN(IDX)
                                       TO   WRK-RJN-FTN(IDX)
                   COMPUTE  WRK-GRP-RJN-FTN(IDX)  =
                            WRK-RJN-FTN(IDX)      -
                            WRK-RUI-RJN-FTN
      *
                   MOVE     LNK-SYU-KOH-FTN(IDX)
                                       TO   WRK-KOH-FTN(IDX)
                   COMPUTE  WRK-GRP-KOH-FTN(IDX)  =
                            WRK-KOH-FTN(IDX)      -
                            WRK-RUI-KOH-FTN
      *
                   MOVE     LNK-SYU-KOH-FTN-ENTANI(IDX)
                                       TO   WRK-KOH-FTN-ENTANI(IDX)
                   COMPUTE  WRK-GRP-KOH-FTN-ENTANI(IDX)  =
                            WRK-KOH-FTN-ENTANI(IDX)      -
                            WRK-RUI-KOH-FTN-ENTANI
      *
                   MOVE     LNK-SYU-HKNTEKMONEY(IDX)
                                       TO   WRK-HKNTEKMONEY(IDX)
                   COMPUTE  WRK-GRP-HKNTEKMONEY(IDX)  =
                            WRK-HKNTEKMONEY(IDX)      -
                            WRK-RUI-HKNTEKMONEY
      *
                   MOVE     LNK-SYU-RSISHOSHIN-MONEY(IDX)
                                       TO   WRK-RSISHOSHIN-MONEY(IDX)
                   COMPUTE  WRK-GRP-RSISHOSHIN-MONEY(IDX)  =
                            WRK-RSISHOSHIN-MONEY(IDX)      -
                            WRK-RUI-RSISHOSHIN-MONEY
      *
                   MOVE     LNK-SYU-RSISAISHIN-MONEY(IDX)
                                       TO   WRK-RSISAISHIN-MONEY(IDX)
                   COMPUTE  WRK-GRP-RSISAISHIN-MONEY(IDX)  =
                            WRK-RSISAISHIN-MONEY(IDX)      -
                            WRK-RUI-RSISAISHIN-MONEY
      *
                   MOVE     LNK-SYU-RSISDO-MONEY(IDX)
                                       TO   WRK-RSISDO-MONEY(IDX)
                   COMPUTE  WRK-GRP-RSISDO-MONEY(IDX)  =
                            WRK-RSISDO-MONEY(IDX)      -
                            WRK-RUI-RSISDO-MONEY
      *
                   MOVE     LNK-SYU-RSIETC-MONEY(IDX)
                                       TO   WRK-RSIETC-MONEY(IDX)
                   COMPUTE  WRK-GRP-RSIETC-MONEY(IDX)  =
                            WRK-RSIETC-MONEY(IDX)      -
                            WRK-RUI-RSIETC-MONEY
      *
                   MOVE     LNK-SYU-RSI-TAX-SAI(IDX)
                                       TO   WRK-RSI-TAX-SAI(IDX)
                   COMPUTE  WRK-GRP-RSI-TAX-SAI(IDX)  =
                            WRK-RSI-TAX-SAI(IDX)      -
                            WRK-RUI-RSI-TAX-SAI
      *
                   MOVE     LNK-SYU-RSI-TOTAL(IDX)
                                       TO   WRK-RSI-TOTAL(IDX)
                   COMPUTE  WRK-GRP-RSI-TOTAL(IDX)  =
                            WRK-RSI-TOTAL(IDX)      -
                            WRK-RUI-RSI-TOTAL
      *
                   MOVE     LNK-SYU-RSIJIBAI-SKYMONEY(IDX)
                                       TO   WRK-RSIJIBAI-SKYMONEY(IDX)
                   COMPUTE  WRK-GRP-RSIJIBAI-SKYMONEY(IDX)  =
                            WRK-RSIJIBAI-SKYMONEY(IDX)      -
                            WRK-RUI-RSIJIBAI-SKYMONEY
      *
                   MOVE     LNK-SYU-SKYMONEY-TAX-SAI(IDX)
                                       TO   WRK-SKYMONEY-TAX-SAI(IDX)
                   COMPUTE  WRK-GRP-SKYMONEY-TAX-SAI(IDX)  =
                            WRK-SKYMONEY-TAX-SAI(IDX)      -
                            WRK-RUI-SKYMONEY-TAX-SAI
      *
                   MOVE     LNK-SYU-JYO-HKNFTNMONEY(IDX)
                                       TO   WRK-TOKUSYU-FTNMONEY(IDX)
                   COMPUTE  WRK-GRP-TOKUSYU-FTNMONEY(IDX)  =
                            WRK-TOKUSYU-FTNMONEY(IDX)      -
                            WRK-RUI-TOKUSYU-FTNMONEY
      *
                   MOVE     LNK-SYU-JYO-COMPFTN(IDX)
                                       TO   WRK-TOKUSYU-COMPFTN(IDX)
                   COMPUTE  WRK-GRP-TOKUSYU-COMPFTN(IDX)  =
                            WRK-TOKUSYU-COMPFTN(IDX)      -
                            WRK-RUI-TOKUSYU-COMPFTN
      *
                   MOVE     LNK-SYU-JYO-COMPFTN-ENTANI(IDX)
                                       TO   WRK-TOKUSYU-ENTANI(IDX)
                   COMPUTE  WRK-GRP-TOKUSYU-ENTANI(IDX)  =
                            WRK-TOKUSYU-ENTANI(IDX)      -
                            WRK-RUI-TOKUSYU-ENTANI
      *
                   MOVE     LNK-SYU-DISCOUNT-MONEY(IDX)
                                       TO   WRK-DISCOUNT-MONEY(IDX)
                   COMPUTE  WRK-GRP-DISCOUNT-MONEY(IDX)  =
                            WRK-DISCOUNT-MONEY(IDX)      -
                            WRK-RUI-DISCOUNT-MONEY
      *
      *
                   MOVE      ZERO      TO      FLG-OK
                   COMPUTE   IDY    =  IDX  -  1
                   PERFORM   VARYING   IDZ   FROM    IDY   BY   -1
                              UNTIL   (IDZ       <       1)
                               OR     (FLG-OK    =       1)
                       COMPUTE  WRK-KEISAN  =  WRK-SKYMONEY(IDZ)
                                            +  WRK-GRP-SGKMONEY(IDX)
                       IF       WRK-KEISAN  <   0
                           CONTINUE
                       ELSE
                           MOVE     WRK-GRP-SGKMONEY(IDX)
                                              TO   WRK-GRP-SGKMONEY(IDZ)
                           MOVE     WRK-KEISAN    TO   WRK-SKYMONEY(IDZ)
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-SYUHKNFTNMONEY(IDZ)  +
                                    WRK-GRP-SYUHKNFTNMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-SYUHKNFTNMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-SYUCOMPFTN(IDZ)  +
                                    WRK-GRP-SYUCOMPFTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-SYUCOMPFTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN2  =
                                    WRK-SYUCOMPFTN-ENTANI(IDZ)  +
                                    WRK-GRP-SYUCOMPFTN-ENTANI(IDX)
                           IF       WRK-KEISAN2  >=   0
                              MOVE    WRK-KEISAN2
                                    TO   WRK-SYUCOMPFTN-ENTANI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-SYUCOMPTOTAL(IDZ)  +
                                    WRK-GRP-SYUCOMPTOTAL(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-SYUCOMPTOTAL(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH1HKNFTNMONEY(IDZ)  +
                                    WRK-GRP-KOH1HKNFTNMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH1HKNFTNMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH1COMPFTN(IDZ)  +
                                    WRK-GRP-KOH1COMPFTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH1COMPFTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN2  =
                                    WRK-KOH1COMPFTN-ENTANI(IDZ)  +
                                    WRK-GRP-KOH1COMPFTN-ENTANI(IDX)
                           IF       WRK-KEISAN2  >=   0
                              MOVE    WRK-KEISAN2
                                    TO   WRK-KOH1COMPFTN-ENTANI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH1COMPTOTAL(IDZ)  +
                                    WRK-GRP-KOH1COMPTOTAL(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH1COMPTOTAL(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH2HKNFTNMONEY(IDZ)  +
                                    WRK-GRP-KOH2HKNFTNMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH2HKNFTNMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH2COMPFTN(IDZ)  +
                                    WRK-GRP-KOH2COMPFTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH2COMPFTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN2  =
                                    WRK-KOH2COMPFTN-ENTANI(IDZ)  +
                                    WRK-GRP-KOH2COMPFTN-ENTANI(IDX)
                           IF       WRK-KEISAN2  >=   0
                              MOVE    WRK-KEISAN2
                                    TO   WRK-KOH2COMPFTN-ENTANI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH2COMPTOTAL(IDZ)  +
                                    WRK-GRP-KOH2COMPTOTAL(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH2COMPTOTAL(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH3HKNFTNMONEY(IDZ)  +
                                    WRK-GRP-KOH3HKNFTNMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH3HKNFTNMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH3COMPFTN(IDZ)  +
                                    WRK-GRP-KOH3COMPFTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH3COMPFTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN2  =
                                    WRK-KOH3COMPFTN-ENTANI(IDZ)  +
                                    WRK-GRP-KOH3COMPFTN-ENTANI(IDX)
                           IF       WRK-KEISAN2  >=   0
                              MOVE    WRK-KEISAN2
                                    TO   WRK-KOH3COMPFTN-ENTANI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH3COMPTOTAL(IDZ)  +
                                    WRK-GRP-KOH3COMPTOTAL(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH3COMPTOTAL(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH4HKNFTNMONEY(IDZ)  +
                                    WRK-GRP-KOH4HKNFTNMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH4HKNFTNMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH4COMPFTN(IDZ)  +
                                    WRK-GRP-KOH4COMPFTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH4COMPFTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN2  =
                                    WRK-KOH4COMPFTN-ENTANI(IDZ)  +
                                    WRK-GRP-KOH4COMPFTN-ENTANI(IDX)
                           IF       WRK-KEISAN2  >=   0
                              MOVE    WRK-KEISAN2
                                    TO   WRK-KOH4COMPFTN-ENTANI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH4COMPTOTAL(IDZ)  +
                                    WRK-GRP-KOH4COMPTOTAL(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH4COMPTOTAL(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-TOTAL-TGMONEY-TAX(IDZ)  +
                                    WRK-GRP-TOTAL-TGMONEY-TAX(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-TOTAL-TGMONEY-TAX(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-TGMONEY-TAX-SAI(IDZ)  +
                                    WRK-GRP-TGMONEY-TAX-SAI(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-TGMONEY-TAX-SAI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-JIHI-TAX(IDZ)  +
                                    WRK-GRP-JIHI-TAX(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-JIHI-TAX(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RJN-FTN(IDZ)  +
                                    WRK-GRP-RJN-FTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RJN-FTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-KOH-FTN(IDZ)  +
                                    WRK-GRP-KOH-FTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-KOH-FTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN2  =
                                    WRK-KOH-FTN-ENTANI(IDZ)  +
                                    WRK-GRP-KOH-FTN-ENTANI(IDX)
                           IF       WRK-KEISAN2  >=   0
                              MOVE    WRK-KEISAN2
                                    TO   WRK-KOH-FTN-ENTANI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-HKNTEKMONEY(IDZ)  +
                                    WRK-GRP-HKNTEKMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-HKNTEKMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RSISHOSHIN-MONEY(IDZ)  +
                                    WRK-GRP-RSISHOSHIN-MONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RSISHOSHIN-MONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RSISAISHIN-MONEY(IDZ)  +
                                    WRK-GRP-RSISAISHIN-MONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RSISAISHIN-MONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RSISDO-MONEY(IDZ)  +
                                    WRK-GRP-RSISDO-MONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RSISDO-MONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RSIETC-MONEY(IDZ)  +
                                    WRK-GRP-RSIETC-MONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RSIETC-MONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RSI-TAX-SAI(IDZ)  +
                                    WRK-GRP-RSI-TAX-SAI(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RSI-TAX-SAI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RSI-TOTAL(IDZ)  +
                                    WRK-GRP-RSI-TOTAL(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RSI-TOTAL(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-RSIJIBAI-SKYMONEY(IDZ)  +
                                    WRK-GRP-RSIJIBAI-SKYMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-RSIJIBAI-SKYMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-SKYMONEY-TAX-SAI(IDZ)  +
                                    WRK-GRP-SKYMONEY-TAX-SAI(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-SKYMONEY-TAX-SAI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-TOKUSYU-FTNMONEY(IDZ)  +
                                    WRK-GRP-TOKUSYU-FTNMONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-TOKUSYU-FTNMONEY(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-TOKUSYU-COMPFTN(IDZ)  +
                                    WRK-GRP-TOKUSYU-COMPFTN(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-TOKUSYU-COMPFTN(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-TOKUSYU-ENTANI(IDZ)  +
                                    WRK-GRP-TOKUSYU-ENTANI(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-TOKUSYU-ENTANI(IDZ)
                           END-IF
      *
                           COMPUTE  WRK-KEISAN  =
                                    WRK-DISCOUNT-MONEY(IDZ)  +
                                    WRK-GRP-DISCOUNT-MONEY(IDX)
                           IF       WRK-KEISAN  >=   0
                              MOVE    WRK-KEISAN
                                    TO   WRK-DISCOUNT-MONEY(IDZ)
                           END-IF
      *
      *
                           MOVE     1         TO   FLG-OK
                       END-IF
                   END-PERFORM
      *
                END-IF
      *
             END-IF
      *
           END-PERFORM.
      *
      *    収納の項目再編集
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX                 >    15 )
                                OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                                OR   (LNK-S01-RC      NOT =  ZERO )
      *
             IF   LNK-SYU-SRYKA(IDX)  NOT =  "00"
      *
      ******    MOVE   WRK-SKYMONEY(IDX)
      ******                        TO   LNK-SYU-SKYMONEY(IDX)
      ******    MOVE   WRK-GRP-SGKMONEY(IDX)
      ******                        TO   LNK-SYU-GRP-SGKMONEY(IDX)
      *
                MOVE   WRK-SYUHKNFTNMONEY(IDX)
                                    TO   LNK-SYU-SYUHKNFTNMONEY(IDX)
                MOVE   WRK-SYUCOMPFTN(IDX)
                                    TO   LNK-SYU-SYUCOMPFTN(IDX)
                MOVE   WRK-SYUCOMPFTN-ENTANI(IDX)
                                    TO   LNK-SYU-SYUCOMPFTN-ENTANI(IDX)
                MOVE   WRK-SYUCOMPTOTAL(IDX)
                                    TO   LNK-SYU-SYUCOMPTOTAL(IDX)
                MOVE   WRK-KOH1HKNFTNMONEY(IDX)
                                    TO   LNK-SYU-KOH1HKNFTNMONEY(IDX)
                MOVE   WRK-KOH1COMPFTN(IDX)
                                    TO   LNK-SYU-KOH1COMPFTN(IDX)
                MOVE   WRK-KOH1COMPFTN-ENTANI(IDX)
                                    TO   LNK-SYU-KOH1COMPFTN-ENTANI(IDX)
                MOVE   WRK-KOH1COMPTOTAL(IDX)
                                    TO   LNK-SYU-KOH1COMPTOTAL(IDX)
                MOVE   WRK-KOH2HKNFTNMONEY(IDX)
                                    TO   LNK-SYU-KOH2HKNFTNMONEY(IDX)
                MOVE   WRK-KOH2COMPFTN(IDX)
                                    TO   LNK-SYU-KOH2COMPFTN(IDX)
                MOVE   WRK-KOH2COMPFTN-ENTANI(IDX)
                                    TO   LNK-SYU-KOH2COMPFTN-ENTANI(IDX)
                MOVE   WRK-KOH2COMPTOTAL(IDX)
                                    TO   LNK-SYU-KOH2COMPTOTAL(IDX)
                MOVE   WRK-KOH3HKNFTNMONEY(IDX)
                                    TO   LNK-SYU-KOH3HKNFTNMONEY(IDX)
                MOVE   WRK-KOH3COMPFTN(IDX)
                                    TO   LNK-SYU-KOH3COMPFTN(IDX)
                MOVE   WRK-KOH3COMPFTN-ENTANI(IDX)
                                    TO   LNK-SYU-KOH3COMPFTN-ENTANI(IDX)
                MOVE   WRK-KOH3COMPTOTAL(IDX)
                                    TO   LNK-SYU-KOH3COMPTOTAL(IDX)
                MOVE   WRK-KOH4HKNFTNMONEY(IDX)
                                    TO   LNK-SYU-KOH4HKNFTNMONEY(IDX)
                MOVE   WRK-KOH4COMPFTN(IDX)
                                    TO   LNK-SYU-KOH4COMPFTN(IDX)
                MOVE   WRK-KOH4COMPFTN-ENTANI(IDX)
                                    TO   LNK-SYU-KOH4COMPFTN-ENTANI(IDX)
                MOVE   WRK-KOH4COMPTOTAL(IDX)
                                    TO   LNK-SYU-KOH4COMPTOTAL(IDX)
                MOVE   WRK-TOTAL-TGMONEY-TAX(IDX)
                                    TO   LNK-SYU-TOTAL-TGMONEY-TAX(IDX)
                MOVE   WRK-TGMONEY-TAX-SAI(IDX)
                                    TO   LNK-SYU-TGMONEY-TAX-SAI(IDX)
                MOVE   WRK-JIHI-TAX(IDX)
                                    TO   LNK-SYU-JIHI-TAX(IDX)
                MOVE   WRK-RJN-FTN(IDX)
                                    TO   LNK-SYU-RJN-FTN(IDX)
                MOVE   WRK-KOH-FTN(IDX)
                                    TO   LNK-SYU-KOH-FTN(IDX)
                MOVE   WRK-KOH-FTN-ENTANI(IDX)
                                    TO   LNK-SYU-KOH-FTN-ENTANI(IDX)
                MOVE   WRK-HKNTEKMONEY(IDX)
                                    TO   LNK-SYU-HKNTEKMONEY(IDX)
                MOVE   WRK-RSISHOSHIN-MONEY(IDX)
                                    TO   LNK-SYU-RSISHOSHIN-MONEY(IDX)
                MOVE   WRK-RSISAISHIN-MONEY(IDX)
                                    TO   LNK-SYU-RSISAISHIN-MONEY(IDX)
                MOVE   WRK-RSISDO-MONEY(IDX)
                                    TO   LNK-SYU-RSISDO-MONEY(IDX)
                MOVE   WRK-RSIETC-MONEY(IDX)
                                    TO   LNK-SYU-RSIETC-MONEY(IDX)
                MOVE   WRK-RSI-TAX-SAI(IDX)
                                    TO   LNK-SYU-RSI-TAX-SAI(IDX)
                MOVE   WRK-RSI-TOTAL(IDX)
                                    TO   LNK-SYU-RSI-TOTAL(IDX)
                MOVE   WRK-RSIJIBAI-SKYMONEY(IDX)
                                    TO   LNK-SYU-RSIJIBAI-SKYMONEY(IDX)
                MOVE   WRK-SKYMONEY-TAX-SAI(IDX)
                                    TO   LNK-SYU-SKYMONEY-TAX-SAI(IDX)
                MOVE   WRK-TOKUSYU-FTNMONEY(IDX)
                                    TO   LNK-SYU-JYO-HKNFTNMONEY(IDX)
                MOVE   WRK-TOKUSYU-COMPFTN(IDX)
                                    TO   LNK-SYU-JYO-COMPFTN(IDX)
                MOVE   WRK-TOKUSYU-ENTANI(IDX)
                                    TO   LNK-SYU-JYO-COMPFTN-ENTANI(IDX)
                MOVE   WRK-DISCOUNT-MONEY(IDX)
                                    TO   LNK-SYU-DISCOUNT-MONEY(IDX)
      *
      ****** ELSE
      *
      ******    IF     LNK-S01-KAGRPFLG     =   "0"
      *
      ******        MOVE   WRK-SKYMONEY(IDX)
      ******                        TO   LNK-SYU-SKYMONEY(IDX)
      ******        MOVE   WRK-GRP-SGKMONEY(IDX)
      ******                        TO   LNK-SYU-GRP-SGKMONEY(IDX)
      *
      ******    END-IF
      *
             END-IF
      *
           END-PERFORM.
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      ******************************************************************
      *    算定
      ******************************************************************
       100-SANTEI-SEC                     SECTION.
      *
           MOVE       SPACE     TO    LNK-S01-GENBUTU-MSG-FLG
           MOVE       ZERO      TO    CNT-KENSU
           MOVE       ZERO      TO    FLG-TOKUSYU-SAI
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX                 >    15 )
                                OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                                OR   (LNK-S01-RC      NOT =  ZERO )
      *
                ADD      1                    TO  CNT-KENSU
      *
                MOVE     WRK-ZAITAKU-KBN      TO
                                      LNK-S01-KOUREI-ZAITAKU-KBN
      *
                MOVE     LNK-ORCS01-REC       TO  WRK-ORCS01-REC
                MOVE     IDX                  TO  WRK-S01-IDX
                MOVE     LNK-SYUNOU-REC(IDX)  TO  SYUNOU-REC
      *
                PERFORM  1001-SANTEI-IDX-HENSYU-SEC
                MOVE     IDZ2                 TO  WRK-S01-IDX2
      *
                MOVE     SPACE                TO  WRK-SYUNOU-REC-T
                INITIALIZE                        WRK-SYUNOU-REC-T
      *
                MOVE     ZERO      TO     IDZ
                IF   SYU-SRYKA  =  "00"
                  PERFORM  VARYING  IDY  FROM  1  BY  1
                            UNTIL  (IDY                 >   15  )
                             OR    (LNK-SYU-SRYYMD(IDY) =  SPACE)
                    IF    IDY  =  IDX
                      CONTINUE
                    ELSE
                      IF   WRK-S01-TTLARIFLG(IDY)  =    "1"
                        IF   LNK-SYU-SRYKA(IDY)       =  "00"
                            ADD   1      TO  IDZ
                            MOVE  LNK-SYUNOU-REC(IDY)
                                         TO  WRK-SYUNOU-REC(IDZ)
                        END-IF
                      ELSE
      *                 当該収納が合計レコードで、
      *                 合計レコードをもたないレコードを読む込む場合
                        IF   LNK-SYU-SRYKA(IDY)   NOT =  "00"
                            ADD   1      TO  IDZ
                            MOVE  LNK-SYUNOU-REC(IDY)
                                         TO  WRK-SYUNOU-REC(IDZ)
                        END-IF
                      END-IF
                    END-IF
                  END-PERFORM
                ELSE
                  PERFORM  VARYING  IDY  FROM  1  BY  1
                            UNTIL  (IDY                 >   15  )
                             OR    (LNK-SYU-SRYYMD(IDY) =  SPACE)
                    IF    IDY  =  IDX
                      CONTINUE
                    ELSE
                      IF  WRK-S01-TTLARIFLG(IDX)  =    "1"
                          IF     LNK-SYU-SRYKA(IDY)   NOT =  "00"
                              ADD   1      TO  IDZ
                              MOVE  LNK-SYUNOU-REC(IDY)
                                           TO  WRK-SYUNOU-REC(IDZ)
                          END-IF
                      ELSE
                        IF   WRK-S01-TTLARIFLG(IDY)  =    "1"
                          IF     WRK-S01-KAGRPFLG       =   "0"
      *                     ＜複数科まとめ集計する場合＞
      *                     当該収納が合計レコードをもたないレコードで、
      *                     合計レコードをもつレコードを読む込む場合
                            IF   LNK-SYU-SRYKA(IDY)       =  "00"
                              ADD   1      TO  IDZ
                              MOVE  LNK-SYUNOU-REC(IDY)
                                           TO  WRK-SYUNOU-REC(IDZ)
                            END-IF
                          ELSE
                            IF   LNK-SYU-SRYKA(IDY)   NOT =  "00"
                              ADD   1      TO  IDZ
                              MOVE  LNK-SYUNOU-REC(IDY)
                                           TO  WRK-SYUNOU-REC(IDZ)
                            END-IF
                          END-IF
                        ELSE
                          IF     LNK-SYU-SRYKA(IDY)   NOT =  "00"
                              ADD   1      TO  IDZ
                              MOVE  LNK-SYUNOU-REC(IDY)
                                           TO  WRK-SYUNOU-REC(IDZ)
                          END-IF
                        END-IF
                      END-IF
                    END-IF
                  END-PERFORM
                END-IF
      *
      *         複数科まとめ集計しない場合、合計レコードは処理しない
                IF   ((WRK-S01-KAGRPFLG   =   "1"   ) AND
                      (SYU-SRYKA          =   "00"  ))
                OR   ( SYU-HKNCOMBINUM    =   "9999" )
                    CONTINUE
                ELSE
                    CALL     "ORCS01"     USING    MCPAREA
                                                   WRK-ORCS01-REC
                                                   SYUNOU-REC
                                                   WRK-SYUNOU-REC-T
                                                   SPA-AREA
                END-IF
      *
                MOVE     SYUNOU-REC           TO  LNK-SYUNOU-REC(IDX)
                MOVE     WRK-ORCS01-REC       TO  LNK-ORCS01-REC
      *
                IF     LNK-S01-TOKUSYU-SAI-KBN  NOT =  SPACE
                     MOVE      1       TO     FLG-TOKUSYU-SAI
                END-IF
      *
           END-PERFORM
      *
           .
      *
       100-SANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    チェック用ＩＤＸ編集
      ******************************************************************
       1001-SANTEI-IDX-HENSYU-SEC             SECTION.
      *
           MOVE     ZERO      TO     IDZ2
      *
           IF       IDX   >   1
             IF   SYU-SRYKA  =  "00"
               PERFORM    VARYING   IDY    FROM   1   BY   1
                           UNTIL    IDY      >    IDX - 1
                 IF   WRK-S01-TTLARIFLG(IDY)  =    "1"
                   IF   LNK-SYU-SRYKA(IDY)       =  "00"
                          ADD   1      TO  IDZ2
                   END-IF
                 ELSE
                   IF   LNK-SYU-SRYKA(IDY)   NOT =  "00"
                          ADD   1      TO  IDZ2
                   END-IF
                 END-IF
               END-PERFORM
             ELSE
               PERFORM    VARYING   IDY    FROM   1   BY   1
                           UNTIL    IDY      >    IDX - 1
                 IF   WRK-S01-TTLARIFLG(IDX)  =    "1"
                   IF   LNK-SYU-SRYKA(IDY)      NOT =  "00"
                          ADD   1      TO  IDZ2
                   END-IF
                 ELSE
                   IF   WRK-S01-TTLARIFLG(IDY)   =   "1"
                     IF   WRK-S01-KAGRPFLG       =   "0"
                       IF   LNK-SYU-SRYKA(IDY)   =  "00"
                          ADD   1      TO  IDZ2
                       END-IF
                     ELSE
                       IF   LNK-SYU-SRYKA(IDY)  NOT =  "00"
                          ADD   1      TO  IDZ2
                       END-IF
                     END-IF
                   ELSE
                     IF   LNK-SYU-SRYKA(IDY)    NOT =  "00"
                          ADD   1      TO  IDZ2
                     END-IF
                   END-IF
                 END-IF
               END-PERFORM
             END-IF
           END-IF
      *
           .
      *
       1001-SANTEI-IDX-HENSYU-EXT.
           EXIT.
      *
