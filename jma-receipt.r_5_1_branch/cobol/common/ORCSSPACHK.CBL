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
       PROGRAM-ID.                 ORCSSPACHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : ＳＰＡ−ＡＲＥＡチェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/07/11    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-ERR             PIC 9(01).
      *
      *    添字領域
      *01  IDX-AREA.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERR            PIC X(80).
           03  WRK-ERRMSG         PIC X(200).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    SPA-ARE数値チェック
           PERFORM 100-SPANUM-CHK-SEC
      *
           IF      WRK-ERR         NOT =   SPACE
               STRING  "SPA-ARE ERR SPA-MOTOPG ["
                                      DELIMITED  BY  SIZE
                       SPA-MOTOPG     DELIMITED  BY  SPACE
                       "] : SPA-SAKIPG [" 
                                      DELIMITED  BY  SIZE
                       SPA-SAKIPG     DELIMITED  BY  SPACE
                       "] :"          DELIMITED  BY  SIZE
                       WRK-ERR        DELIMITED  BY  "@"
                       LOW-VALUE      DELIMITED  BY  SIZE
                       INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"      USING
                                       WRK-ERRMSG
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    SPA-ARE数値チェック処理
      *****************************************************************
       100-SPANUM-CHK-SEC         SECTION.
      *
      *医療機関識別番号
           IF    SPA-HOSPNUM(1:2)      NOT NUMERIC
               MOVE    "SPA-HOSPNUM ERR @"   TO  WRK-ERR
           END-IF
      *病院種別
           IF    SPA-HOSPSBT(1:1)      NOT NUMERIC
               MOVE    "SPA-HOSPSBT ERR @"  TO  WRK-ERR
           END-IF
      *県番号
           IF    SPA-PREFNUM(1:2)      NOT NUMERIC
               MOVE    "SPA-PREFNUM ERR @"   TO  WRK-ERR
           END-IF
      *外来時間外区分
           IF    SPA-GAI-JIKANKBN(1:1) NOT NUMERIC
               MOVE    "SPA-GAI-JIKANKBN ERR @"   TO  WRK-ERR
           END-IF
      *点数マスタ更新区分
           IF    SPA-DBUPDKBN(1:1)      NOT NUMERIC
               MOVE    "SPA-DBUPDKBN ERR @"   TO  WRK-ERR
           END-IF
      *病床数
           IF    SPA-BEDSU(1:4)         NOT NUMERIC
               MOVE    "SPA-BEDSU ERR @"   TO  WRK-ERR
           END-IF
      *病床数（一般）
           IF    SPA-BEDSUIPN(1:4)      NOT NUMERIC
               MOVE    "SPA-BEDSUIPN ERR @"   TO  WRK-ERR
           END-IF
      *排他フラグ
           IF    SPA-HAITAFLG(1:1)      NOT NUMERIC
               MOVE    "SPA-HAITAFLG ERR @"   TO  WRK-ERR
           END-IF
      *無床・有床区分（０：無床版、１：有床版）
           IF    SPA-BEDKBN(1:1)      NOT NUMERIC
               MOVE    "SPA-BEDKBN ERR @"   TO  WRK-ERR
           END-IF
      *病床数フラグ（０：病床無し、１：病床有り）
           IF    SPA-BEDFLG(1:1)      NOT NUMERIC
               MOVE    "SPA-BEDFLG ERR @"   TO  WRK-ERR
           END-IF
      *データ作成フラグ
           IF    SPA-DATACREATEFLG(1:1)      NOT NUMERIC
               MOVE    "SPA-DATACREATEFLG ERR @"   TO  WRK-ERR
           END-IF
      *データ提出方法区分
           IF    SPA-DATATEISYUTUKBN(1:1)      NOT NUMERIC
               MOVE    "SPA-DATATEISYUTUKBN ERR @"   TO  WRK-ERR
           END-IF
      *前回患者ＩＤ
           IF    SPA-LAST-PTID(1:10)      NOT NUMERIC
               MOVE    "SPA-LAST-PTID ERR @"   TO  WRK-ERR
           END-IF
      *数量０入力フラグ
           IF    SPA-SURYOZERO-FLG(1:1)   NOT NUMERIC
               MOVE    "SPA-SURYOZERO-FLG ERR @"   TO  WRK-ERR
           END-IF
      *処理フラグ（Ｕ・Ｐ起動）
           IF    SPA-SYORI-FLG(1:1)   NOT NUMERIC
               MOVE    "SPA-SYORI-FLG ERR @"   TO  WRK-ERR
           END-IF
      *排他中
           IF    SPA-LOCK(1:1)   NOT NUMERIC
               MOVE    "SPA-LOCK ERR @"   TO  WRK-ERR
           END-IF
      *患者ＩＤ
           IF    SPA-PTID(1:10)   NOT NUMERIC
               MOVE    "SPA-PTID ERR @"   TO  WRK-ERR
           END-IF
      *負担率
           IF    SPA-FTNRATE(1:3)  NOT NUMERIC
               MOVE    "SPA-FTNRATE ERR @"   TO  WRK-ERR
           END-IF
      *受付時間ＩＤ
           IF      SPA-P02-UKEID(1:5)   NOT NUMERIC
               MOVE    "SPA-P02-UKEID ERR @"   TO  WRK-ERR
           END-IF
      *受付選択番号
           IF      SPA-P02-U02(1:4)  NOT NUMERIC
               MOVE    "SPA-P02-U02 ERR @"   TO  WRK-ERR
           END-IF
      *保険区分（０：一般、１：労災・自賠責）
           IF    SPA-HKNKBN(1:1)  NOT NUMERIC
               MOVE    "SPA-HKNKBN ERR @"   TO  WRK-ERR
           END-IF
      *四肢区分（労災用）
           IF    SPA-RSI-SISIKBN(1:1)  NOT NUMERIC
               MOVE    "SPA-RSI-SISIKBN ERR @"   TO  WRK-ERR
           END-IF
      *ジョブ管理ＤＢのキー値  
           IF     SPA-JOB-JOBID(1:7)  NOT NUMERIC
               MOVE    "SPA-JOB-JOBID ERR @"   TO  WRK-ERR
           END-IF
      *
      *患者番号構成管理情報
      *患者番号構成区分によりチェック
           EVALUATE    SPA-1009-PTNUMKSIKBN
               WHEN    "1"
                  IF       SPA-1009-JIYKSIKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-JIYKSIKETA ERR @"
                                                     TO  WRK-ERR
                   END-IF
               WHEN    "2"
      *    標準構成連番号桁数
                  IF       SPA-1009-HJNKSIRENNUMKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-HJNKSIRENNUMKETA ERR @" 
                                                   TO  WRK-ERR
                   END-IF
               WHEN    "3"
      *    拡張構成前桁数
                  IF       SPA-1009-KKCKSIMAEKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-KKCKSIMAEKETA ERR @"
                                                   TO  WRK-ERR
                   END-IF
      *    拡張構成連番号桁数
                  IF       SPA-1009-KKCKSIRENNUMKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-KKCKSIRENNUMKETA ERR @"
                                                   TO  WRK-ERR
                   END-IF
      *    拡張構成後桁数
                  IF       SPA-1009-KKCKSIATOKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-KKCKSIATOKETA ERR @"
                                                   TO  WRK-ERR
                   END-IF
           END-EVALUATE
           .
       100-SPANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *
