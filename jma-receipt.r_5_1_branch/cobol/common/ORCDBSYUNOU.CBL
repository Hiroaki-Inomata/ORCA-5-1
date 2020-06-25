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
       PROGRAM-ID.             ORCDBSYUNOU.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : ORCA DB 収納アクセスモジュール
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/24    NACL-太田     新規作成
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
           03  FLG-SYUMAIN         PIC 9(01).
           03  FLG-SYUNYUIN        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
      *
       01  CONST-AREA.
           03  CONST-KOH-MAX           PIC 9(05)   VALUE 4.
           03  CONST-SKY-MAX           PIC 9(05)   VALUE 17.
           03  CONST-JIH-MAX           PIC 9(05)   VALUE 10.
           03  CONST-KYUFUGAI-MAX      PIC 9(05)   VALUE 6.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納マスタ（新）
       01  SYUMAIN-REC.
           COPY    "CPSYUMAIN.INC".
      *
      *    入院収納マスタ
       01  SYUNYUIN-REC.
           COPY    "CPSYUNYUIN.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           MCPDATA-REC
           SPA-AREA.
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
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-EDIT-SYUNOU-SEC
      *
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ編集処理
      *****************************************************************
       200-EDIT-SYUNOU-SEC             SECTION.
      *
           MOVE    "tbl_syunou_main"   TO  MCP-TABLE
           MOVE    MCPDATA-REC         TO  SYUNOU-REC
      *
           EVALUATE    MCP-FUNC
           WHEN    "DBSELECT"
               PERFORM 2001-EDIT-SYUMAIN-SEC
               MOVE    SYUMAIN-REC     TO  MCPDATA-REC
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
           WHEN    "DBCLOSECURSOR"
               PERFORM 910-DBCLOSECURSOR-SEC
           WHEN    "DBFETCH"
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
               IF    ( MCP-RC  =   ZERO )
                   PERFORM 2001-SYUMAIN-FET-SEC
               END-IF
           WHEN    "DBUPDATE"
               PERFORM 2001-SYUMAIN-UPD-SEC
           WHEN    "DBDELETE"
               PERFORM 2001-SYUMAIN-DEL-SEC
           WHEN    "DBINSERT"
               PERFORM 2001-SYUMAIN-INS-SEC
           END-EVALUATE
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
      *
           .
       200-EDIT-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    新収納マスタ（主）編集処理
      *****************************************************************
       2001-EDIT-SYUMAIN-SEC          SECTION.
      *
           INITIALIZE                      SYUMAIN-REC
           MOVE    SYU-HOSPNUM         TO  SYUMAIN-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SYUMAIN-NYUGAIKBN
           MOVE    SYU-PTID            TO  SYUMAIN-PTID
           MOVE    SYU-DENPNUM         TO  SYUMAIN-DENPNUM
           MOVE    SYU-DENPLASTNUM     TO  SYUMAIN-DENPLASTNUM
           MOVE    SYU-SRYKA           TO  SYUMAIN-SRYKA
           MOVE    SYU-SRYYMD          TO  SYUMAIN-SRYYMD
           MOVE    SYU-DENPJTIKBN      TO  SYUMAIN-DENPJTIKBN
           MOVE    SYU-CREATEKBN       TO  SYUMAIN-CREATEKBN
           MOVE    SYU-DENPPRTYMD      TO  SYUMAIN-DENPPRTYMD
           MOVE    SYU-SKYSTYMD        TO  SYUMAIN-SKYSTYMD
           MOVE    SYU-SKYEDYMD        TO  SYUMAIN-SKYEDYMD
           MOVE    SYU-HKNCOMBINUM     TO  SYUMAIN-HKNCOMBINUM
           MOVE    SYU-SYUHKNNUM       TO  SYUMAIN-SYUHKNNUM
           MOVE    SYU-SYUHKNFTNMONEY  TO  SYUMAIN-SYUHKNFTNMONEY
           MOVE    SYU-SYUCOMPFTN      TO  SYUMAIN-SYUCOMPFTN
           MOVE    SYU-SYUCOMPFTN-ENTANI
                                       TO  SYUMAIN-SYUCOMPFTN-ENTANI
           MOVE    SYU-SYUCOMPYKZFTN   TO  SYUMAIN-SYUCOMPYKZFTN
           MOVE    SYU-SYUCOMPTOTAL    TO  SYUMAIN-SYUCOMPTOTAL
           MOVE    SYU-SYUYKZFTNKBN    TO  SYUMAIN-SYUYKZFTNKBN
      *
           PERFORM VARYING IDX1 FROM    1   BY  1
                   UNTIL ( IDX1 >   CONST-KOH-MAX )
      *
               MOVE    SYU-KOH-HKNNUM  (IDX1)
                                   TO  SYUMAIN-KOH-HKNNUM (IDX1)
               MOVE    SYU-KOH-HKNFTNMONEY (IDX1)
                                   TO  SYUMAIN-KOH-HKNFTNMONEY(IDX1)
               MOVE    SYU-KOH-COMPFTN (IDX1)
                                   TO  SYUMAIN-KOH-COMPFTN    (IDX1)
               MOVE    SYU-KOH-COMPFTN-ENTANI (IDX1)
                                   TO  SYUMAIN-KOH-COMPFTN-ENTANI (IDX1)
               MOVE    SYU-KOH-COMPYKZFTN (IDX1)
                                   TO  SYUMAIN-KOH-COMPYKZFTN (IDX1)
               MOVE    SYU-KOH-COMPTOTAL (IDX1)
                                   TO  SYUMAIN-KOH-COMPTOTAL  (IDX1)
               MOVE    SYU-KOH-YKZFTNKBN (IDX1)
                                   TO  SYUMAIN-KOH-YKZFTNKBN  (IDX1)
      *
           END-PERFORM
      *
           MOVE    SYU-PTFTNRATE   TO  SYUMAIN-PTFTNRATE
           MOVE    SYU-SKYKUKBN    TO  SYUMAIN-SKYKUKBN
           MOVE    SYU-TAX-TAISHOU TO  SYUMAIN-TAX-TAISHOU
           MOVE    SYU-TAX-MONEY   TO  SYUMAIN-TAX-MONEY
           MOVE    SYU-SKYGK       TO  SYUMAIN-SKYGK
      *
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   CONST-SKY-MAX )
      *
               MOVE    SYU-HKNTEN (IDX1)
                                       TO  SYUMAIN-HKNTEN (IDX1)
               MOVE    SYU-MONEY  (IDX1)
                                       TO  SYUMAIN-MONEY  (IDX1)
               MOVE    SYU-TGMONEY(IDX1)
                                       TO  SYUMAIN-TGMONEY(IDX1)
               MOVE    SYU-TGMONEY-TAX (IDX1)
                                       TO  SYUMAIN-TGMONEY-TAX (IDX1)
      *
           END-PERFORM
           MOVE    SYU-SHOHOU-SAI      TO  SYUMAIN-SHOHOU-SAI
           MOVE    SYU-TGMONEY-TAX-SAI TO  SYUMAIN-TGMONEY-TAX-SAI
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   CONST-JIH-MAX )
               MOVE    SYU-JIHI-TAXNASI (IDX1)
                                       TO  SYUMAIN-JIHI-TAXNASI(IDX1)
               MOVE    SYU-JIHI-TAXARI  (IDX1)
                                       TO  SYUMAIN-JIHI-TAXARI (IDX1)
           END-PERFORM
           MOVE    SYU-JIHI-TOTAL      TO  SYUMAIN-JIHI-TOTAL
           MOVE    SYU-JIHI-TOTAL-TAX  TO  SYUMAIN-JIHI-TOTAL-TAX
           MOVE    SYU-JIHI-TAX        TO  SYUMAIN-JIHI-TAX
           MOVE    SYU-RJN-FTN         TO  SYUMAIN-RJN-FTN
           MOVE    SYU-KOH-FTN         TO  SYUMAIN-KOH-FTN
           MOVE    SYU-KOH-FTN-ENTANI  TO  SYUMAIN-KOH-FTN-ENTANI
           MOVE    SYU-YKZ-FTN         TO  SYUMAIN-YKZ-FTN
           MOVE    SYU-RESE-YKZ-FTN    TO  SYUMAIN-RESE-YKZ-FTN
           MOVE    SYU-KOHTAIYKZKBN    TO  SYUMAIN-KOHTAIYKZKBN
           MOVE    SYU-CHOSEI          TO  SYUMAIN-CHOSEI
           MOVE    SYU-CHOSEI1         TO  SYUMAIN-CHOSEI1
           MOVE    SYU-CHOSEI2         TO  SYUMAIN-CHOSEI2
           MOVE    SYU-GRP-DENPNUM     TO  SYUMAIN-GRP-DENPNUM
           MOVE    SYU-GRP-RENNUM      TO  SYUMAIN-GRP-RENNUM
           MOVE    SYU-GRP-SGKMONEY    TO  SYUMAIN-GRP-SGKMONEY
           MOVE    SYU-GRP-HAKHOUFLG   TO  SYUMAIN-GRP-HAKHOUFLG
           MOVE    SYU-SECMONEY        TO  SYUMAIN-SECMONEY
      *
           IF      SYU-NYUGAIKBN    =  "2"
             IF    SYU-ACCT-UPDKBN  =  SPACE
               IF  SYU-SECMONEY     =   8
                 MOVE    ZERO          TO  SYUMAIN-SECMONEY
               END-IF
             ELSE
               IF  SYU-SECMONEY     =  ZERO OR 9
                 MOVE     8            TO  SYUMAIN-SECMONEY
               END-IF
             END-IF
           END-IF
      *
           MOVE    SYU-HKNTEKMONEY     TO  SYUMAIN-HKNTEKMONEY
           MOVE    SYU-DISCOUNT-KBN    TO  SYUMAIN-DISCOUNT-KBN
           MOVE    SYU-DISCOUNT-BODY   TO  SYUMAIN-DISCOUNT-BODY
           MOVE    SYU-DISCOUNT-RATEKBN
                                       TO  SYUMAIN-DISCOUNT-RATEKBN
           MOVE    SYU-DISCOUNT-TEIRITU
                                       TO  SYUMAIN-DISCOUNT-TEIRITU
           MOVE    SYU-DISCOUNT-RATE   TO  SYUMAIN-DISCOUNT-RATE
           MOVE    SYU-DISCOUNT-MONEY  TO  SYUMAIN-DISCOUNT-MONEY
           MOVE    SYU-RSISHOSHIN-MONEY
                                       TO  SYUMAIN-RSISHOSHIN-MONEY
           MOVE    SYU-RSISAISHIN-MONEY
                                       TO  SYUMAIN-RSISAISHIN-MONEY
           MOVE    SYU-RSISDO-MONEY    TO  SYUMAIN-RSISDO-MONEY
           MOVE    SYU-RSIETC-MONEY    TO  SYUMAIN-RSIETC-MONEY
           MOVE    SYU-RSI-TAX-SAI     TO  SYUMAIN-RSI-TAX-SAI
           MOVE    SYU-RSI-TOTAL       TO  SYUMAIN-RSI-TOTAL
           MOVE    SYU-RSIJIBAI-SKYMONEY
                                       TO  SYUMAIN-RSIJIBAI-SKYMONEY
           MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  SYUMAIN-SKYMONEY-TAX-SAI
           MOVE    SYU-SKYMONEY        TO  SYUMAIN-SKYMONEY
           MOVE    SYU-NYUKIN-TOTAL    TO  SYUMAIN-NYUKIN-TOTAL
           MOVE    SYU-NYUKIN-KAISU    TO  SYUMAIN-NYUKIN-KAISU
           MOVE    SYU-MISYURIYU       TO  SYUMAIN-MISYURIYU
           MOVE    SYU-DRCD-G          TO  SYUMAIN-DRCD-G
           MOVE    SYU-NYNSHOKAISU     TO  SYUMAIN-NYNSHOKAISU
           MOVE    SYU-YKZKENNUM       TO  SYUMAIN-YKZKENNUM
           MOVE    SYU-SKJ-FTNGAKU(1)  TO  SYUMAIN-SKJ-FTNGAKU(1)
           MOVE    SYU-SKJ-FTNDAY (1)  TO  SYUMAIN-SKJ-FTNDAY (1)
           MOVE    SYU-SKJ-FTNGAKU(2)  TO  SYUMAIN-SKJ-FTNGAKU(2)
           MOVE    SYU-SKJ-FTNDAY (2)  TO  SYUMAIN-SKJ-FTNDAY (2)
           MOVE    SYU-SKJ-FTNGAKU(3)  TO  SYUMAIN-SKJ-FTNGAKU(3)
           MOVE    SYU-SKJ-FTNDAY (3)  TO  SYUMAIN-SKJ-FTNDAY (3)
           MOVE    SYU-SAIKEISANKBN    TO  SYUMAIN-SAIKEISANKBN
           MOVE    SYU-INGAISHOHOKBN   TO  SYUMAIN-INGAISHOHOKBN
           MOVE    SYU-DOUJI-DENPNUM   TO  SYUMAIN-DOUJI-DENPNUM
           MOVE    SYU-CONTKBN         TO  SYUMAIN-CONTKBN
           MOVE    SYU-FUKU-DENPNUM    TO  SYUMAIN-FUKU-DENPNUM
           MOVE    SYU-FUKU-KBN        TO  SYUMAIN-FUKU-KBN
           MOVE    SYU-ZAITAKU         TO  SYUMAIN-ZAITAKU
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >      6 )
               MOVE    SYU-KYUFUGAI-TEN (IDX1)
                                       TO SYUMAIN-KYUFUGAI-TEN (IDX1)
           END-PERFORM
           MOVE    SYU-DAYINFFLG       TO  SYUMAIN-DAYINFFLG
           MOVE    SYU-BUNBENINFFLG    TO  SYUMAIN-BUNBENINFFLG
           MOVE    SYU-ACCT-UPDKBN     TO  SYUMAIN-ACCT-UPDKBN
           MOVE    SYU-JYO-HKNFTNMONEY TO  SYUMAIN-JYO-HKNFTNMONEY
           MOVE    SYU-JYO-COMPFTN     TO  SYUMAIN-JYO-COMPFTN
           MOVE    SYU-JYO-COMPFTN-ENTANI
                                       TO  SYUMAIN-JYO-COMPFTN-ENTANI
           IF    ( SYU-SKYMONEY        =   SYU-NYUKIN-TOTAL )
               MOVE    "0"             TO  SYUMAIN-SKYNYUKINKBN
           ELSE
               MOVE    "1"             TO  SYUMAIN-SKYNYUKINKBN
           END-IF
           MOVE    SYU-HKNTEKKBN       TO  SYUMAIN-HKNTEKKBN
           MOVE    SYU-TERMID          TO  SYUMAIN-TERMID
           MOVE    SYU-OPID            TO  SYUMAIN-OPID
           MOVE    SYU-CREYMD          TO  SYUMAIN-CREYMD
           MOVE    SYU-UPYMD           TO  SYUMAIN-UPYMD
           MOVE    SYU-UPHMS           TO  SYUMAIN-UPHMS
           MOVE    SYU-UPSRYYMD        TO  SYUMAIN-UPSRYYMD
           MOVE    SYU-NYUIN-RRKNUM    TO  SYUMAIN-SELNYUIN-RRKNUM
           MOVE    SYU-SELKOHID        TO  SYUMAIN-SELKOHID
           MOVE    SYU-SELNYUGAIKBN    TO  SYUMAIN-SELNYUGAIKBN
      *
           .
       2001-EDIT-SYUMAIN-EXT.
           EXIT.
      *****************************************************************
      *    新収納マスタ（主）FETCH処理
      *****************************************************************
       2001-SYUMAIN-FET-SEC            SECTION.
      *
           MOVE    MCPDATA-REC         TO  SYUMAIN-REC
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SYUMAIN-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SYUMAIN-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SYUMAIN-PTID            TO  SYU-PTID
           MOVE    SYUMAIN-DENPNUM         TO  SYU-DENPNUM
           MOVE    SYUMAIN-DENPLASTNUM     TO  SYU-DENPLASTNUM
           MOVE    SYUMAIN-SRYKA           TO  SYU-SRYKA
           MOVE    SYUMAIN-SRYYMD          TO  SYU-SRYYMD
           MOVE    SYUMAIN-DENPJTIKBN      TO  SYU-DENPJTIKBN
           MOVE    SYUMAIN-CREATEKBN       TO  SYU-CREATEKBN
           MOVE    SYUMAIN-DENPPRTYMD      TO  SYU-DENPPRTYMD
           MOVE    SYUMAIN-SKYSTYMD        TO  SYU-SKYSTYMD
           MOVE    SYUMAIN-SKYEDYMD        TO  SYU-SKYEDYMD
           MOVE    SYUMAIN-HKNCOMBINUM     TO  SYU-HKNCOMBINUM
           MOVE    SYUMAIN-SYUHKNNUM       TO  SYU-SYUHKNNUM
           MOVE    SYUMAIN-SYUHKNFTNMONEY  TO  SYU-SYUHKNFTNMONEY
           MOVE    SYUMAIN-SYUCOMPFTN      TO  SYU-SYUCOMPFTN
           MOVE    SYUMAIN-SYUCOMPFTN-ENTANI
                                       TO  SYU-SYUCOMPFTN-ENTANI
           MOVE    SYUMAIN-SYUCOMPYKZFTN   TO  SYU-SYUCOMPYKZFTN
           MOVE    SYUMAIN-SYUCOMPTOTAL    TO  SYU-SYUCOMPTOTAL
           MOVE    SYUMAIN-SYUYKZFTNKBN    TO  SYU-SYUYKZFTNKBN
      *
           PERFORM VARYING IDX1 FROM    1   BY  1
                   UNTIL ( IDX1 >   CONST-KOH-MAX )
      *
               MOVE    SYUMAIN-KOH-HKNNUM  (IDX1)
                                   TO  SYU-KOH-HKNNUM (IDX1)
               MOVE    SYUMAIN-KOH-HKNFTNMONEY (IDX1)
                                   TO  SYU-KOH-HKNFTNMONEY(IDX1)
               MOVE    SYUMAIN-KOH-COMPFTN (IDX1)
                                   TO  SYU-KOH-COMPFTN    (IDX1)
               MOVE    SYUMAIN-KOH-COMPFTN-ENTANI (IDX1)
                                   TO  SYU-KOH-COMPFTN-ENTANI (IDX1)
               MOVE    SYUMAIN-KOH-COMPYKZFTN (IDX1)
                                   TO  SYU-KOH-COMPYKZFTN (IDX1)
               MOVE    SYUMAIN-KOH-COMPTOTAL (IDX1)
                                   TO  SYU-KOH-COMPTOTAL  (IDX1)
               MOVE    SYUMAIN-KOH-YKZFTNKBN (IDX1)
                                   TO  SYU-KOH-YKZFTNKBN  (IDX1)
      *
           END-PERFORM
      *
           MOVE    SYUMAIN-PTFTNRATE   TO  SYU-PTFTNRATE
           MOVE    SYUMAIN-SKYKUKBN    TO  SYU-SKYKUKBN
           MOVE    SYUMAIN-TAX-TAISHOU TO  SYU-TAX-TAISHOU
           MOVE    SYUMAIN-TAX-MONEY   TO  SYU-TAX-MONEY
           MOVE    SYUMAIN-SKYGK       TO  SYU-SKYGK
      *
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   CONST-SKY-MAX )
      *
               MOVE    SYUMAIN-HKNTEN (IDX1)
                                       TO  SYU-HKNTEN (IDX1)
               MOVE    SYUMAIN-MONEY  (IDX1)
                                       TO  SYU-MONEY  (IDX1)
               MOVE    SYUMAIN-TGMONEY(IDX1)
                                       TO  SYU-TGMONEY(IDX1)
               MOVE    SYUMAIN-TGMONEY-TAX (IDX1)
                                       TO  SYU-TGMONEY-TAX (IDX1)
      *
           END-PERFORM
           MOVE    SYUMAIN-SHOHOU-SAI  TO  SYU-SHOHOU-SAI
           MOVE    SYUMAIN-TGMONEY-TAX-SAI
                                       TO  SYU-TGMONEY-TAX-SAI
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   CONST-JIH-MAX )
               MOVE    SYUMAIN-JIHI-TAXNASI (IDX1)
                                       TO  SYU-JIHI-TAXNASI(IDX1)
               MOVE    SYUMAIN-JIHI-TAXARI  (IDX1)
                                       TO  SYU-JIHI-TAXARI (IDX1)
           END-PERFORM
           MOVE    SYUMAIN-JIHI-TOTAL      TO  SYU-JIHI-TOTAL
           MOVE    SYUMAIN-JIHI-TOTAL-TAX  TO  SYU-JIHI-TOTAL-TAX
           MOVE    SYUMAIN-JIHI-TAX        TO  SYU-JIHI-TAX
           MOVE    SYUMAIN-RJN-FTN         TO  SYU-RJN-FTN
           MOVE    SYUMAIN-KOH-FTN         TO  SYU-KOH-FTN
           MOVE    SYUMAIN-KOH-FTN-ENTANI  TO  SYU-KOH-FTN-ENTANI
           MOVE    SYUMAIN-YKZ-FTN         TO  SYU-YKZ-FTN
           MOVE    SYUMAIN-RESE-YKZ-FTN    TO  SYU-RESE-YKZ-FTN
           MOVE    SYUMAIN-KOHTAIYKZKBN    TO  SYU-KOHTAIYKZKBN
           MOVE    SYUMAIN-CHOSEI          TO  SYU-CHOSEI
           MOVE    SYUMAIN-CHOSEI1         TO  SYU-CHOSEI1
           MOVE    SYUMAIN-CHOSEI2         TO  SYU-CHOSEI2
           MOVE    SYUMAIN-GRP-DENPNUM     TO  SYU-GRP-DENPNUM
           MOVE    SYUMAIN-GRP-RENNUM      TO  SYU-GRP-RENNUM
           MOVE    SYUMAIN-GRP-SGKMONEY    TO  SYU-GRP-SGKMONEY
           MOVE    SYUMAIN-GRP-HAKHOUFLG   TO  SYU-GRP-HAKHOUFLG
           MOVE    SYUMAIN-SECMONEY        TO  SYU-SECMONEY
      *
           IF      SYUMAIN-NYUGAIKBN    =  "2"
             IF    SYUMAIN-ACCT-UPDKBN  =  SPACE
               IF  SYUMAIN-SECMONEY     =   8
                 MOVE    ZERO              TO  SYU-SECMONEY
               END-IF
             ELSE
               IF  SYUMAIN-SECMONEY     =  ZERO OR 9
                 MOVE     8                TO  SYU-SECMONEY
               END-IF
             END-IF
           END-IF
      *
           MOVE    SYUMAIN-HKNTEKMONEY     TO  SYU-HKNTEKMONEY
           MOVE    SYUMAIN-DISCOUNT-KBN    TO  SYU-DISCOUNT-KBN
           MOVE    SYUMAIN-DISCOUNT-BODY   TO  SYU-DISCOUNT-BODY
           MOVE    SYUMAIN-DISCOUNT-RATEKBN
                                       TO  SYU-DISCOUNT-RATEKBN
           MOVE    SYUMAIN-DISCOUNT-TEIRITU
                                       TO  SYU-DISCOUNT-TEIRITU
           MOVE    SYUMAIN-DISCOUNT-RATE   TO  SYU-DISCOUNT-RATE
           MOVE    SYUMAIN-DISCOUNT-MONEY  TO  SYU-DISCOUNT-MONEY
           MOVE    SYUMAIN-RSISHOSHIN-MONEY
                                       TO  SYU-RSISHOSHIN-MONEY
           MOVE    SYUMAIN-RSISAISHIN-MONEY
                                       TO  SYU-RSISAISHIN-MONEY
           MOVE    SYUMAIN-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           MOVE    SYUMAIN-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           MOVE    SYUMAIN-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           MOVE    SYUMAIN-RSI-TOTAL       TO  SYU-RSI-TOTAL
           MOVE    SYUMAIN-RSIJIBAI-SKYMONEY
                                       TO  SYU-RSIJIBAI-SKYMONEY
           MOVE    SYUMAIN-SKYMONEY-TAX-SAI
                                       TO  SYU-SKYMONEY-TAX-SAI
           MOVE    SYUMAIN-SKYMONEY        TO  SYU-SKYMONEY
           MOVE    SYUMAIN-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL
           MOVE    SYUMAIN-NYUKIN-KAISU    TO  SYU-NYUKIN-KAISU
           MOVE    SYUMAIN-MISYURIYU       TO  SYU-MISYURIYU
           MOVE    SYUMAIN-DRCD-G          TO  SYU-DRCD-G
           MOVE    SYUMAIN-NYNSHOKAISU     TO  SYU-NYNSHOKAISU
           MOVE    SYUMAIN-YKZKENNUM       TO  SYU-YKZKENNUM
           MOVE    SYUMAIN-SKJ-FTNGAKU(1)  TO  SYU-SKJ-FTNGAKU(1)
           MOVE    SYUMAIN-SKJ-FTNDAY (1)  TO  SYU-SKJ-FTNDAY (1)
           MOVE    SYUMAIN-SKJ-FTNGAKU(2)  TO  SYU-SKJ-FTNGAKU(2)
           MOVE    SYUMAIN-SKJ-FTNDAY (2)  TO  SYU-SKJ-FTNDAY (2)
           MOVE    SYUMAIN-SKJ-FTNGAKU(3)  TO  SYU-SKJ-FTNGAKU(3)
           MOVE    SYUMAIN-SKJ-FTNDAY (3)  TO  SYU-SKJ-FTNDAY (3)
           MOVE    SYUMAIN-SAIKEISANKBN    TO  SYU-SAIKEISANKBN
           MOVE    SYUMAIN-INGAISHOHOKBN   TO  SYU-INGAISHOHOKBN
           MOVE    SYUMAIN-DOUJI-DENPNUM   TO  SYU-DOUJI-DENPNUM
           MOVE    SYUMAIN-CONTKBN         TO  SYU-CONTKBN
           MOVE    SYUMAIN-FUKU-DENPNUM    TO  SYU-FUKU-DENPNUM
           MOVE    SYUMAIN-FUKU-KBN        TO  SYU-FUKU-KBN
           MOVE    SYUMAIN-ZAITAKU         TO  SYU-ZAITAKU
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   CONST-KYUFUGAI-MAX )
               MOVE    SYUMAIN-KYUFUGAI-TEN (IDX1)
                                       TO SYU-KYUFUGAI-TEN (IDX1)
           END-PERFORM
           MOVE    SYUMAIN-DAYINFFLG       TO  SYU-DAYINFFLG
           MOVE    SYUMAIN-BUNBENINFFLG    TO  SYU-BUNBENINFFLG
           MOVE    SYUMAIN-ACCT-UPDKBN     TO  SYU-ACCT-UPDKBN
           MOVE    SYUMAIN-JYO-HKNFTNMONEY TO  SYU-JYO-HKNFTNMONEY
           MOVE    SYUMAIN-JYO-COMPFTN     TO  SYU-JYO-COMPFTN
           MOVE    SYUMAIN-JYO-COMPFTN-ENTANI
                                       TO  SYU-JYO-COMPFTN-ENTANI
           MOVE    SYUMAIN-HKNTEKKBN       TO  SYU-HKNTEKKBN
           MOVE    SYUMAIN-TERMID          TO  SYU-TERMID
           MOVE    SYUMAIN-OPID            TO  SYU-OPID
           MOVE    SYUMAIN-CREYMD          TO  SYU-CREYMD
           MOVE    SYUMAIN-UPYMD           TO  SYU-UPYMD
           MOVE    SYUMAIN-UPHMS           TO  SYU-UPHMS
           MOVE    SYUMAIN-UPSRYYMD        TO  SYU-UPSRYYMD
           MOVE    SYUMAIN-SELKOHID        TO  SYU-SELKOHID
           MOVE    SYUMAIN-SELNYUGAIKBN    TO  SYU-SELNYUGAIKBN
      *
      *    "3"には定期請求差額を求めるときに一時的に置き換えている
           IF    ( SYUMAIN-NYUGAIKBN       =   "1" OR "3" )
                   PERFORM 20011-SYUNYUIN-GET-SEC
           END-IF
      *
           MOVE    ZERO                    TO  MCP-RC
      *
           .
       2001-SYUMAIN-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ（入院情報）取得処理
      *****************************************************************
       20011-SYUNYUIN-GET-SEC          SECTION.
      *
           PERFORM 900-SYUNYUIN-KEY-SEL-SEC
      *
           MOVE    SYUNYUIN-ROOMNUM    TO  SYU-ROOMNUM
           MOVE    SYUNYUIN-BYOTONUM   TO  SYU-BYOTONUM
           MOVE    SYUNYUIN-RMSAGAKU   TO  SYU-RMSAGAKU
           MOVE    SYUNYUIN-RMSAGAKU-TAX-SAI
                                       TO  SYU-RMSAGAKU-TAX-SAI
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   14 )
               MOVE    SYUNYUIN-SHOKUJI-NISSU (IDX1)
                                       TO  SYU-SHOKUJI-NISSU (IDX1)
               MOVE    SYUNYUIN-SHOKUJI (IDX1)
                                       TO  SYU-SHOKUJI (IDX1)
           END-PERFORM
           MOVE    SYUNYUIN-SYUSKJ-NISSU   TO  SYU-SYUSKJ-NISSU
           MOVE    SYUNYUIN-SYUSKJ-RYOYOHI TO  SYU-SYUSKJ-RYOYOHI
           MOVE    SYUNYUIN-SYUSKJ-FTN     TO  SYU-SYUSKJ-FTN
           MOVE    SYUNYUIN-SYUSKJ-FTN-RECE TO SYU-SYUSKJ-FTN-RECE
           MOVE    SYUNYUIN-SYUSKJ-FTNKBN
                                       TO  SYU-SYUSKJ-FTNKBN
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   CONST-KOH-MAX )
               MOVE    SYUNYUIN-SKJ-NISSU (IDX1)
                                       TO  SYU-SKJ-NISSU (IDX1)
               MOVE    SYUNYUIN-SKJ-RYOYOHI (IDX1)
                                       TO  SYU-SKJ-RYOYOHI (IDX1)
               MOVE    SYUNYUIN-SKJ-FTN (IDX1)
                                       TO  SYU-SKJ-FTN (IDX1)
               MOVE    SYUNYUIN-SKJ-FTN-RECE (IDX1)
                                       TO  SYU-SKJ-FTN-RECE (IDX1)
               MOVE    SYUNYUIN-SKJ-FTNKBN (IDX1)
                                       TO  SYU-SKJ-FTNKBN (IDX1)
           END-PERFORM
      *
           MOVE    SYUNYUIN-RYOYOHI-SKJ
                                       TO  SYU-RYOYOHI-SKJ
           MOVE    SYUNYUIN-SKYMONEY-SKJ
                                       TO  SYU-SKYMONEY-SKJ
           MOVE    SYUNYUIN-SKYMONEY-SKJ-TAX
                                       TO  SYU-SKYMONEY-SKJ-TAX
           MOVE    SYUNYUIN-SKYMONEY-SKJ-KEI
                                       TO  SYU-SKYMONEY-SKJ-KEI
           MOVE    SYUNYUIN-RYOYOHI-SKJ-JIHI
                                       TO  SYU-RYOYOHI-SKJ-JIHI
           MOVE    SYUNYUIN-SKYMONEY-SKJ-JIHI
                                       TO  SYU-SKYMONEY-SKJ-JIHI
           MOVE    SYUNYUIN-SKYMONEY-SKJ-JIHI-TAX
                                       TO  SYU-SKYMONEY-SKJ-JIHI-TAX
           MOVE    SYUNYUIN-SKYMONEY-SKJ-JIHI-KEI
                                       TO  SYU-SKYMONEY-SKJ-JIHI-KEI
           MOVE    SYUNYUIN-RYOYOHI-LIFE
                                       TO  SYU-RYOYOHI-LIFE
           MOVE    SYUNYUIN-SKYMONEY-LIFE
                                       TO  SYU-SKYMONEY-LIFE
           MOVE    SYUNYUIN-SKYMONEY-LIFE-TAX
                                       TO  SYU-SKYMONEY-LIFE-TAX
           MOVE    SYUNYUIN-SKYMONEY-LIFE-KEI
                                       TO  SYU-SKYMONEY-LIFE-KEI
           MOVE    SYUNYUIN-RYOYOHI-LIFE-JIHI
                                       TO  SYU-RYOYOHI-LIFE-JIHI
           MOVE    SYUNYUIN-SKYMONEY-LIFE-JIHI
                                       TO  SYU-SKYMONEY-LIFE-JIHI
           MOVE    SYUNYUIN-SKYMONEY-LIFE-JIHI-TAX
                                       TO  SYU-SKYMONEY-LIFE-JIHI-TAX
           MOVE    SYUNYUIN-SKYMONEY-LIFE-JIHI-KEI
                                       TO  SYU-SKYMONEY-LIFE-JIHI-KEI
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   31 )
                   MOVE    SYUNYUIN-DAY (IDX1)
                                       TO  SYU-DAY (IDX1)
           END-PERFORM
           MOVE    SYUNYUIN-NYUIN-RRKNUM
                                       TO  SYU-NYUIN-RRKNUM
           MOVE    SYUNYUIN-NYUINETC1  TO  SYU-NYUINETC1
           MOVE    SYUNYUIN-NYUINETC2  TO  SYU-NYUINETC2
           MOVE    SYUNYUIN-NYUINETC3  TO  SYU-NYUINETC3
           MOVE    SYUNYUIN-NYUINETC4  TO  SYU-NYUINETC4
           MOVE    SYUNYUIN-NYUINETC5  TO  SYU-NYUINETC5
      *
           .
       20011-SYUNYUIN-GET-EXT.
           EXIT.
      *****************************************************************
      *    新収納マスタ（主）更新処理
      *****************************************************************
       2001-SYUMAIN-UPD-SEC            SECTION.
      *
           PERFORM 2001-EDIT-SYUMAIN-SEC
      *
           EVALUATE    WRK-PATHNAME
           WHEN    "upd1"
           WHEN    "upd3"
               PERFORM 2001-SYUNYUIN-UPD-SEC
           WHEN    "upd2"
               CONTINUE
           WHEN    OTHER
               PERFORM 2001-SYUNYUIN-INS-SEC
           END-EVALUATE
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SYUMAIN-REC         TO  MCPDATA-REC
               MOVE    "tbl_syunou_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
           END-IF
      *
           .
       2001-SYUMAIN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    新収納マスタ（主）追加処理
      *****************************************************************
       2001-SYUMAIN-INS-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SYUMAIN-SEC
      *
           PERFORM 2001-SYUNYUIN-INS-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SYUMAIN-REC         TO  MCPDATA-REC
               MOVE    "tbl_syunou_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
           END-IF
      *
           .
       2001-SYUMAIN-INS-EXT.
           EXIT.
      *****************************************************************
      *    新収納マスタ（主）削除処理
      *****************************************************************
       2001-SYUMAIN-DEL-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SYUMAIN-SEC
      *
           PERFORM 2001-SYUNYUIN-DEL-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SYUMAIN-REC         TO  MCPDATA-REC
               MOVE    "tbl_syunou_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           END-IF
      *
           .
       2001-SYUMAIN-INS-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ（入院情報）更新処理
      *****************************************************************
       2001-SYUNYUIN-UPD-SEC              SECTION.
      *
           INITIALIZE                      SYUNYUIN-REC
           PERFORM 20011-SYUNYUIN-EDIT-SEC
      *
           MOVE    SYUNYUIN-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou_nyuin"    TO  MCP-TABLE
           MOVE    WRK-PATHNAME          TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *
           .
       2001-SYUNYUIN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ（入院情報）削除処理
      *****************************************************************
       2001-SYUNYUIN-DEL-SEC              SECTION.
      *
           INITIALIZE                      SYUNYUIN-REC
           PERFORM 20011-SYUNYUIN-EDIT-SEC
      *
           MOVE    SYUNYUIN-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou_nyuin"    TO  MCP-TABLE
           MOVE    WRK-PATHNAME          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-SYUNYUIN-DEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ（入院情報）追加処理
      *****************************************************************
       2001-SYUNYUIN-INS-SEC              SECTION.
      *
           PERFORM 2001-SYUNYUIN-DEL-SEC
      *
      *    "3"には定期請求差額を求めるときに一時的に置き換えている
           IF    ( SYU-NYUGAIKBN   =     "1" OR "3" )
               PERFORM 20011-SYUNYUIN-EDIT-SEC
      *
               IF    ( MCP-RC          =    ZERO )
                   MOVE    SYUNYUIN-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syunou_nyuin"  TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
               END-IF
           END-IF
      *
           .
       2001-SYUNYUIN-INS-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ（入院情報）編集処理
      *****************************************************************
       20011-SYUNYUIN-EDIT-SEC           SECTION.
      *
           INITIALIZE                          SYUNYUIN-REC
           PERFORM 9001-SYUNYUIN-KEY-EDIT-SEC
      *
           MOVE    SYU-ROOMNUM         TO  SYUNYUIN-ROOMNUM
           MOVE    SYU-BYOTONUM        TO  SYUNYUIN-BYOTONUM
           MOVE    SYU-RMSAGAKU        TO  SYUNYUIN-RMSAGAKU
           MOVE    SYU-RMSAGAKU-TAX-SAI
                                       TO  SYUNYUIN-RMSAGAKU-TAX-SAI
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   14 )
               MOVE    SYU-SHOKUJI-NISSU (IDX1)
                                       TO  SYUNYUIN-SHOKUJI-NISSU (IDX1)
               MOVE    SYU-SHOKUJI (IDX1)
                                       TO  SYUNYUIN-SHOKUJI (IDX1)
           END-PERFORM
           MOVE    SYU-SYUSKJ-NISSU    TO  SYUNYUIN-SYUSKJ-NISSU
           MOVE    SYU-SYUSKJ-RYOYOHI  TO  SYUNYUIN-SYUSKJ-RYOYOHI
           MOVE    SYU-SYUSKJ-FTN      TO  SYUNYUIN-SYUSKJ-FTN
           MOVE    SYU-SYUSKJ-FTN-RECE TO  SYUNYUIN-SYUSKJ-FTN-RECE
           MOVE    SYU-SYUSKJ-FTNKBN   TO  SYUNYUIN-SYUSKJ-FTNKBN
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   CONST-KOH-MAX )
               MOVE    SYU-SKJ-NISSU (IDX1)
                                       TO  SYUNYUIN-SKJ-NISSU (IDX1)
               MOVE    SYU-SKJ-RYOYOHI (IDX1)
                                       TO  SYUNYUIN-SKJ-RYOYOHI (IDX1)
               MOVE    SYU-SKJ-FTN (IDX1)
                                       TO  SYUNYUIN-SKJ-FTN (IDX1)
               MOVE    SYU-SKJ-FTN-RECE (IDX1)
                                       TO  SYUNYUIN-SKJ-FTN-RECE (IDX1)
               MOVE    SYU-SKJ-FTNKBN (IDX1)
                                       TO  SYUNYUIN-SKJ-FTNKBN (IDX1)
           END-PERFORM
      *
           MOVE    SYU-RYOYOHI-SKJ     TO  SYUNYUIN-RYOYOHI-SKJ
           MOVE    SYU-SKYMONEY-SKJ    TO  SYUNYUIN-SKYMONEY-SKJ
           MOVE    SYU-SKYMONEY-SKJ-TAX
                                   TO  SYUNYUIN-SKYMONEY-SKJ-TAX
           MOVE    SYU-SKYMONEY-SKJ-KEI
                                   TO  SYUNYUIN-SKYMONEY-SKJ-KEI
           MOVE    SYU-RYOYOHI-SKJ-JIHI
                                   TO  SYUNYUIN-RYOYOHI-SKJ-JIHI
           MOVE    SYU-SKYMONEY-SKJ-JIHI
                                   TO  SYUNYUIN-SKYMONEY-SKJ-JIHI
           MOVE    SYU-SKYMONEY-SKJ-JIHI-TAX
                                   TO  SYUNYUIN-SKYMONEY-SKJ-JIHI-TAX
           MOVE    SYU-SKYMONEY-SKJ-JIHI-KEI
                                   TO  SYUNYUIN-SKYMONEY-SKJ-JIHI-KEI
           MOVE    SYU-RYOYOHI-LIFE
                                   TO  SYUNYUIN-RYOYOHI-LIFE
           MOVE    SYU-SKYMONEY-LIFE
                                   TO  SYUNYUIN-SKYMONEY-LIFE
           MOVE    SYU-SKYMONEY-LIFE-TAX
                                   TO  SYUNYUIN-SKYMONEY-LIFE-TAX
           MOVE    SYU-SKYMONEY-LIFE-KEI
                                   TO  SYUNYUIN-SKYMONEY-LIFE-KEI
           MOVE    SYU-RYOYOHI-LIFE-JIHI
                                   TO  SYUNYUIN-RYOYOHI-LIFE-JIHI
           MOVE    SYU-SKYMONEY-LIFE-JIHI
                                   TO  SYUNYUIN-SKYMONEY-LIFE-JIHI
           MOVE    SYU-SKYMONEY-LIFE-JIHI-TAX
                                   TO  SYUNYUIN-SKYMONEY-LIFE-JIHI-TAX
           MOVE    SYU-SKYMONEY-LIFE-JIHI-KEI
                                   TO  SYUNYUIN-SKYMONEY-LIFE-JIHI-KEI
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >   31 )
                   MOVE    SYU-DAY (IDX1)
                                       TO  SYUNYUIN-DAY (IDX1)
           END-PERFORM
           MOVE    SYU-NYUIN-RRKNUM    TO  SYUNYUIN-NYUIN-RRKNUM
           MOVE    SYU-NYUINETC1       TO  SYUNYUIN-NYUINETC1
           MOVE    SYU-NYUINETC2       TO  SYUNYUIN-NYUINETC2
           MOVE    SYU-NYUINETC3       TO  SYUNYUIN-NYUINETC3
           MOVE    SYU-NYUINETC4       TO  SYUNYUIN-NYUINETC4
           MOVE    SYU-NYUINETC5       TO  SYUNYUIN-NYUINETC5
           MOVE    SYU-SRYYMD          TO  SYUNYUIN-SELSRYYMD
           MOVE    SYU-CREATEKBN       TO  SYUNYUIN-SELCREATEKBN
           MOVE    SYU-SELNYUGAIKBN    TO  SYUNYUIN-SELNYUGAIKBN
           .
       20011-SYUNYUIN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院収納マスタ検索処理
      *****************************************************************
       900-SYUNYUIN-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNYUIN
      *
           INITIALIZE                      SYUNYUIN-REC
           PERFORM 9001-SYUNYUIN-KEY-EDIT-SEC
           MOVE    SYUNYUIN-REC        TO  MCPDATA-REC
           MOVE    "tbl_syunou_nyuin"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNYUIN-REC
           ELSE
               MOVE    1               TO  FLG-SYUNYUIN
               INITIALIZE                  SYUNYUIN-REC
           END-IF
      *
           MOVE    "tbl_syunou_nyuin"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNYUIN-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院収納マスタキー情報編集処理
      *****************************************************************
       9001-SYUNYUIN-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SYUMAIN-HOSPNUM     TO  SYUNYUIN-HOSPNUM
           MOVE    SYUMAIN-NYUGAIKBN   TO  SYUNYUIN-NYUGAIKBN
           MOVE    SYUMAIN-PTID        TO  SYUNYUIN-PTID
           MOVE    SYUMAIN-DENPNUM     TO  SYUNYUIN-DENPNUM
      *
           .
       9001-SYUNYUIN-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
       990-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
