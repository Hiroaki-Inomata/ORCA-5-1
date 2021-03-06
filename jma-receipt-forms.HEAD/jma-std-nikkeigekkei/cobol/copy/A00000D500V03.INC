      *******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
      *  vOC³ð
      * Maj/Min/Rev  C³Ò       út      àe
      *  02.00.01    @@@@-åÔ    15/01/27  VKì¬
      *  02.00.02    @@@@-åÔ    15/06/09  ©ï¬vÌóC³
      ********************************************************************
       01  HC17V03.
      *    VXeút
           03  HC17V03-SYSYMD              PIC X(22).
      *    fÃút
           03  HC17V03-SRYYMD              PIC X(22).
      *    Å
           03  HC17V03-PAGE                PIC X(03).
      *    ©oµ
           03  HC17V03-MIDASI              PIC X(10).
      *    WubZ[W
           03  HC17V03-JOBMSG              PIC X(60).
      *    üOæª
           03  HC17V03-NYUGAIKBN           PIC X(04).
      *    fÃÈ
           03  HC17V03-SRYKA               PIC X(20).
      *    ©ïe[u
           03  HC17V03-TJIHI-G             OCCURS 10 TIMES.
      *    ©ï¼Ì
               05  HC17V03-TJIHI           PIC X(08).
      *    AÔe[u
           03  HC17V03-RENNUM-G            OCCURS 21 TIMES.
      *    AÔ
               05  HC17V03-RENNUM          PIC X(07).
      *    ³ÒÔe[u
           03  HC17V03-PTNUM-G             OCCURS 21 TIMES.
      *    ³ÒÔiPOÈºj
               05  HC17V03-PTNUM           PIC X(10).
      *    ³ÒÔiPPÈãj
               05  HC17V03-PTNUM2          PIC X(20).
      *    ³Ò¼e[u
           03  HC17V03-NAME-G              OCCURS 21 TIMES.
      *    ³Ò¼
               05  HC17V03-NAME            PIC X(18).
      *    Û¯_e[u
           03  HC17V03-HKNTEN-G            OCCURS 21 TIMES.
      *    Û¯_
               05  HC17V03-HKNTEN          PIC X(07).
      *    Wvæªe[u
           03  HC17V03-MONEY-G             OCCURS 20 TIMES.
      *    û¿ze[uiWvæªÊj
               05  HC17V03-MONEY-OCC       OCCURS 21 TIMES.
      *    û¿ziWvæªÊj
                   07  HC17V03-MONEY       PIC X(07).
      *    ãÃûve[u
           03  HC17V03-SYUEKI-G            OCCURS 21 TIMES.
      *    ãÃûv
               05  HC17V03-SYUEKI          PIC X(08).
      *    û¿ze[u
           03  HC17V03-SEIKYU-G            OCCURS 21 TIMES.
      *    û¿z
               05  HC17V03-SEIKYU          PIC X(08).
      *    ûÌûze[u
           03  HC17V03-RYOSYU-G            OCCURS 21 TIMES.
      *    ûÌûz
               05  HC17V03-RYOSYU          PIC X(08).
      *    Û¯_\¬viVj
           03  HC17V03-G-HKNTEN            PIC X(07).
      *    Û¯_\¬viWj
           03  HC17V03-GG-HKNTEN           PIC X(08).
      *    ãÃûv\¬viWj
           03  HC17V03-G-SYUEKI            PIC X(08).
      *    ãÃûv\¬viXj
           03  HC17V03-GG-SYUEKI           PIC X(09).
      *    Wvæªe[u\¬v
           03  HC17V03-G-MONEY-TBL         OCCURS 20 TIMES.
      *    û¿z\¬viWvæªÊjiVj
               05  HC17V03-G-MONEY         PIC X(07).
      *    Wvæªe[u\¬v
           03  HC17V03-GG-MONEY-TBL        OCCURS 20 TIMES.
      *    û¿z\¬viWvæªÊjiWj
               05  HC17V03-GG-MONEY        PIC X(08).
      *    û¿z\¬viWj
           03  HC17V03-G-SEIKYU            PIC X(08).
      *    û¿z\¬viXj
           03  HC17V03-GG-SEIKYU           PIC X(09).
      *    ûÌûz\¬viWj
           03  HC17V03-G-RYOSYU            PIC X(08).
      *    ûÌûz\¬viXj
           03  HC17V03-GG-RYOSYU           PIC X(09).
      *    ©ï\¬v
           03  HC17V03-JIHIKEI-TBL         OCCURS 21 TIMES.
               05  HC17V03-JIHI-KEI          PIC X(07).
      *    ©ï\¬viVj
           03  HC17V03-G-JIHIKEI           PIC X(07).
      *    ©ï\¬viWj
           03  HC17V03-GG-JIHIKEI          PIC X(08).
