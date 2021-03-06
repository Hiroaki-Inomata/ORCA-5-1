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
      *----------------------------------------------------------*
      *    �������μ����Ǽ�Խ��ѥ�᥿  COPY (CPORCHC03SUB.INC)
      *                                          (ORCHC03SUB �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCHC03SUBAREA.
      *����
           03  ORCHC03SUB-MAX          PIC 9(04).
      *
           03  ORCHC03SUB-SYUNOU-TBL       OCCURS  15.
      *        ��Ǽ�ޥ���
               05  ORCHC03SUB-SYUNOU-REC       PIC X(2000).
      *        �������
               05  ORCHC03SUB-SKYMONEY         PIC S9(07).
               05  ORCHC03SUB-SKYMONEY-TAX-SAI PIC S9(07).
               05  ORCHC03SUB-NYUKIN-TOTAL     PIC S9(07).
               05  ORCHC03SUB-HENKIN           PIC S9(07).
      *        Ʊ�����Ų�
               05  ORCHC03SUB-DOUJI-SRYKA-G.
                   07  ORCHC03SUB-DOUJI-SRYKA  PIC  X(02)
                                               OCCURS  10.
      *        Ʊ���ݸ��ȹ礻
               05  ORCHC03SUB-DOUJI-HKNCOMBI-G.
                   07  ORCHC03SUB-DOUJI-HKNCOMBI   PIC  X(04)
                                               OCCURS  10.
      *        Ʊ����ɼ�ֹ�ʿ��������ٽ��ѡ�
               05  ORCHC03SUB-DOUJI-DENPNUM-G.
                   07  ORCHC03SUB-DOUJI-DENPNUM    PIC  9(07)
                                               OCCURS  15.
      *
