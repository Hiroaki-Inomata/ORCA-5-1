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
      *  �ץ�����ཤ������
      * Maj/Min/Rev       ������    ����          ����
      * 01.00.01          ���      08/09/25      ����ǯ����ɲ�
      * 01.00.02          ����      09/09/02      Ģɼ�ͼ����ѹ���ȼ������
      * 01.00.03          ���      18/12/04      �������򥳥��Ⱥ��
      *                                           ��׷����4�������
      ********************************************************************
       01  SC07.
      *����ǯ��
         03  SC07-SRYYM            PIC  X(16).
      *���ŵ��إ�����
         03  SC07-HOSPCD           PIC  X(07).
      *͹���ֹ�
         03  SC07-POST             PIC  X(16).
      *����
         03  SC07-ADRS             PIC  X(100).
      *���ŵ���̾
         03  SC07-HOSPNAME         PIC  X(100).
      *���߼Ի�̾
         03  SC07-KAISETUNAME      PIC  X(40).
      *�����ֹ�
         03  SC07-TEL              PIC  X(15).
      *����ɽ��ʬ
         03  SC07-TENSUHYO         PIC  X(02)  OCCURS 3.
      *���
         03  SC07-KENSU            PIC  X(04).
