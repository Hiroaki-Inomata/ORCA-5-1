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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *---------------------------------------------------*
      *    ��ʬ����ٻ�����ѥ�᥿                          *
      *                         COPY  (CPORCSSANFUKA.INC) *
      *
      *---------------------------------------------------*
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  ORCSSANFUKAAREA.
      *�ɣι���
      *������ʬ�ʣ�������ʬ���ס����������׻�������������
           03  LNK-SANFUKA-SYORI             PIC  9(01).
      *�����У�
           03  LNK-SANFUKA-PG                PIC  X(03).
      *����ǯ����
           03  LNK-SANFUKA-SRYYMD            PIC  X(08).
      *    �������ο��ť�����
           03  LNK-SANFUKA-TEISEI-AREA.
               05  LNK-SANFUKA-TEISEI-OCC      OCCURS   400.
                   07  LNK-SANFUKA-TEI-SRYCD   PIC X(09).
                   07  LNK-SANFUKA-TEI-KAISU   PIC 9(03).
           03  LNK-SANFUKA-I-O.
      *��ʬ����ٻ�������
               05  LNK-SANFUKA-TEN-MAX         PIC 9(08).
      *����ʬ���
               05  LNK-SANFUKA-MSANTESU        PIC 9(08).
      *����ǽ�������
               05  LNK-SANFUKA-MSANTEID        PIC 9(02).
      *����ʬ���
               05  LNK-SANFUKA-MSANTESU-DEL    PIC 9(08).
      *
           03  LNK-SANFUKA-OUT.
      *�����������顼
               05  LNK-SANFUKA-ERRMSG       PIC X(80).
      *
