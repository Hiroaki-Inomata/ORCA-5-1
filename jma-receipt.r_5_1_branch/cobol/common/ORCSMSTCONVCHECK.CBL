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
       PROGRAM-ID.             ORCSMSTCONVCHECK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 起動時マスタデータ移行チェック結果取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/12/17    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  IN01-FILE       ASSIGN  IN01PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN01.
       
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       FD  IN01-FILE.
       01  IN01-REC                PIC X(50).
      *
       WORKING-STORAGE             SECTION.
      *
       01  STS-AREA.
           03  STS-IN01            PIC X(02).
      *
       01  WRK-AREA.
           03  IN01PARA            PIC X(200).
      *
       01  CONST-AREA.
           03  CONST-MC-FILE       PIC X(48)   VALUE
               "/var/lib/jma-receipt/master/master_convert_check".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
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
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
      *
           MOVE    CONST-MC-FILE       TO  IN01PARA
           OPEN    INPUT   IN01-FILE
           IF      STS-IN01            =   ZERO
               MOVE    "1022"              TO  SPA-ERRCD
      *        READ    IN01-FILE
      *            AT  END
      *        END-READ
           END-IF
           CLOSE   IN01-FILE
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
