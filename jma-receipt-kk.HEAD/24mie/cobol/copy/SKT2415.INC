      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      ********************************************************************
      *    ������������� �ʻ��Ÿ���     COPY (SKT2415.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/25    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
        01  SKT2415.
            03  SKT2415-SRYYM           PIC X(16).
            03  SKT2415-SEIYMD          PIC X(22).
            03  SKT2415-HOSPCD          PIC X(20).
            03  SKT2415-HKNJANUM        PIC X(4).
            03  SKT2415-ADRS            PIC X(100).
            03  SKT2415-HOSPNAME        PIC X(100).
            03  SKT2415-TEL             PIC X(15).
            03  SKT2415-KAISETUNAME     PIC X(40).
            03  SKT2415-ZAI1            PIC X(2).
            03  SKT2415-ZAI2            PIC X(2).
            03  SKT2415-NO              PIC X(4).
            03  SKT2415-SEIKYU-TBL      OCCURS  3 TIMES.
              05  SKT2415-NYUGAI-TBL    OCCURS  2 TIMES.
                07  SKT2415-KENSU       PIC X(5).
                07  SKT2415-NISSU       PIC X(5).
                07  SKT2415-TENSU       PIC X(9).
                07  SKT2415-ITBFTN      PIC X(9).
              05  SKT2415-S-KENSU       PIC X(5).
              05  SKT2415-S-NISSU       PIC X(5).
              05  SKT2415-S-KINGK       PIC X(9).
              05  SKT2415-S-FTNGK       PIC X(9).
            03  SKT2415-KOUHI-TBL       OCCURS  5 TIMES.
              05  SKT2415-HBTNUM        PIC X(2).
              05  SKT2415-KOHNAME       PIC X(8).
              05  SKT2415-K-NYUGAI-TBL  OCCURS  2 TIMES.
                07  SKT2415-KKENSU      PIC X(5).
                07  SKT2415-KNISSU      PIC X(5).
                07  SKT2415-KTENSU      PIC X(9).
                07  SKT2415-KITBFTN     PIC X(9).
              05  SKT2415-S-KKENSU      PIC X(5).
              05  SKT2415-S-KNISSU      PIC X(5).
              05  SKT2415-S-KKINGK      PIC X(9).
              05  SKT2415-S-KFTNGK      PIC X(9).