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
      *    �������ե�����     COPY (CPSKT2610TMP.INC)
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/11/02  ��������������б�
      *  01.00.02    ����         10/11/24  open-cobol1.0�б�(¿���������)
      *  01.00.03    ����         11/04/22  ����������ʿ�β��Ͽ��б�
      *  01.00.04    ���         18/12/04  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2610TMP-REC.
           03  SKT2610TMP-KEY.
               05  SKT2610TMP-NYUGAIKBN        PIC  9(01).
           03  SKT2610TMP-DATA   OCCURS  11.
               05  SKT2610TMP-NYUGAI-TBL   OCCURS  2.
                   07  SKT2610TMP-KENSU        PIC  9(06).
                   07  SKT2610TMP-NISSU        PIC  9(06).
                   07  SKT2610TMP-TENSU        PIC  9(10).
                   07  SKT2610TMP-ITBFTN       PIC  9(10).
                   07  SKT2610TMP-TYOKI        PIC  9(02).
                   07  SKT2610TMP-GENBAKU      PIC  9(02).
                   07  SKT2610TMP-S-KENSU      PIC  9(06).
                   07  SKT2610TMP-S-NISSU      PIC  9(06).
                   07  SKT2610TMP-S-KINGAKU    PIC  9(10).
                   07  SKT2610TMP-S-FTNGAKU    PIC  9(10).
           03  SKT2610TMP-GENMEN-TBL.
               05  SKT2610TMP-G-KENSU          PIC  9(03).
               05  SKT2610TMP-G-TENSU          PIC  9(07).
           03  SKT2610TMP-T-KENSU              PIC  9(03).
           03  SKT2610TMP-T-TENSU              PIC  9(10).
           03  SKT2610TMP-SAIGAI               PIC  X(01).
           03  SKT2610TMP-SAIGAI-TBL       OCCURS  2.
               05  SKT2610TMP-SAI-KENSU        PIC  9(06).
               05  SKT2610TMP-SAI-TENSU        PIC  9(10).
