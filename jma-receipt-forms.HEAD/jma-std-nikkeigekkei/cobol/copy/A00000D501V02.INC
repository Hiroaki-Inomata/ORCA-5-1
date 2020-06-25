      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
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
      *****************************************************************
      *  �v���O�����C������
      * Maj/Min/Rev  �C����       ���t      ���e
      *  01.00.00    @@@@-���    15/01/27  �V�K�쐬
      ********************************************************************
       01  HC17BV02.
            03  HC17BV02-SYSYMD      PIC X(22).
            03  HC17BV02-SRYYMD      PIC X(22).
            03  HC17BV02-PAGE        PIC  X(3).
            03  HC17BV02-MIDASI      PIC X(10).
            03  HC17BV02-JOBMSG      PIC X(60).
            03  HC17BV02-NYUGAIKBN   PIC X(4).
            03  HC17BV02-SRYKA       PIC X(20).
            03  HC17BV02-TJIHI-G     OCCURS  10 TIMES.
                05  HC17BV02-TJIHI   PIC X(8).
            03  HC17BV02-KENSU1
                    PIC X(8)      OCCURS  2 TIMES.
            03  HC17BV02-KENSU2
                    PIC X(8)      OCCURS  2 TIMES.
            03  HC17BV02-KENSU3
                    PIC X(8)      OCCURS  2 TIMES.
            03  HC17BV02-KENSU4
                    PIC X(10)     OCCURS  2 TIMES.
            03  HC17BV02-TJYUSHIN    PIC X(8).
            03  HC17BV02-GHKNKEN     PIC X(9).
            03  HC17BV02-OHKNKEN     PIC X(9).
            03  HC17BV02-RHKNKEN     PIC X(9).
            03  HC17BV02-SHKNKEN     PIC X(9).
            03  HC17BV02-SAHKNKEN    PIC X(9).
            03  HC17BV02-GMONEY-G    OCCURS  20 TIMES.
              05  HC17BV02-GMONEY    PIC X(9).
            03  HC17BV02-OKENSU-G    OCCURS  20 TIMES.
              05  HC17BV02-OKENSU    PIC X(9).
            03  HC17BV02-RKENSU-G    OCCURS  20 TIMES.
              05  HC17BV02-RKENSU    PIC X(9).
            03  HC17BV02-SKENSU-G    OCCURS  20 TIMES.
              05  HC17BV02-SKENSU    PIC X(9).
            03  HC17BV02-SAKENSU-G   OCCURS  20 TIMES.
              05  HC17BV02-SAKENSU   PIC X(9).
            03  HC17BV02-GSEIKYU     PIC X(9).
            03  HC17BV02-GRYOSYU     PIC X(9).
            03  HC17BV02-GSYUEKI     PIC X(9).
            03  HC17BV02-OSEIKYUKEN  PIC X(9).
            03  HC17BV02-ORYOSYUKEN  PIC X(9).
            03  HC17BV02-OSYUEKIKEN  PIC X(9).
            03  HC17BV02-RSEIKYUKEN  PIC X(9).
            03  HC17BV02-RRYOSYUKEN  PIC X(9).
            03  HC17BV02-RSYUEKIKEN  PIC X(9).
            03  HC17BV02-SSEIKYUKEN  PIC X(9).
            03  HC17BV02-SRYOSYUKEN  PIC X(9).
            03  HC17BV02-SSYUEKIKEN  PIC X(9).
            03  HC17BV02-SASEIKYUKEN PIC X(9).
            03  HC17BV02-SARYOSYUKEN PIC X(9).
            03  HC17BV02-SASYUEKIKEN PIC X(9).
            03  HC17BV02-JYUSHINCNT  PIC X(9).
            03  HC17BV02-JIHI-GMONEY  PIC X(9).
            03  HC17BV02-JIHI-OKENSU  PIC X(9).
            03  HC17BV02-JIHI-RKENSU  PIC X(9).
            03  HC17BV02-JIHI-SKENSU  PIC X(9).
            03  HC17BV02-JIHI-SAKENSU  PIC X(9).