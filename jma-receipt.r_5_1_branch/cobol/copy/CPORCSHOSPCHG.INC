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
      ************************************************************
      *    ���ŵ��إ��������ؤ�Ƚ��ѥ�᥿������COPY (CPORCSHOSPCHG.INC)
      *                                ���ѥץ������ (ORCSHOSPCHG)
      *    �������
      *      ���ŵ��أɣ�
      *      Ƚ��ǯ��
      *      ���ԣɣ�
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSHOSPCHG-AREA.
      *
      *���ŵ��أɣġ�ɬ�ܡ�
           03  LNK-HOSPCHG-HOSPNUM                   PIC  9(02).
      *Ƚ��ǯ���ɬ�ܡ�
           03  LNK-HOSPCHG-HANTEIYM                  PIC  X(06).
      *���ԣɣġ�Ǥ�ա�
           03  LNK-HOSPCHG-PTID                      PIC  9(10).
      *�ֵѹ���
           03  LNK-HOSPCHG-HENKYAKU-AREA.
      *���ŵ��إ��������ؤ���ʬ�ʣ������ŵ��إ��������ؤ��ʤ�
      *                          �������ŵ��إ��������ؤ������
             05  LNK-HOSPCHG-CHGKBN                  PIC  9(01).
      *���ŵ��إ������ֵѥ��ꥢ��ź����������ŵ��ء�ź�����������ŵ��ء�
             05  LNK-HOSPCHG-HENKYAKU-TABLE          OCCURS  2.
      *���ŵ��إ�����
               07  LNK-HOSPCHG-HOSPCD                PIC  9(07).
      *���ŵ��إ����ɡʴ�����
               07  LNK-HOSPCHG-HOSPCDN               PIC  X(40).
      *���ŵ��ؼ���
               07  LNK-HOSPCHG-HOSPSBT               PIC  9(01).
      *ͭ������ǯ����
               07  LNK-HOSPCHG-STYUKYMD              PIC  X(08).
      *ͭ����λǯ����
               07  LNK-HOSPCHG-EDYUKYMD              PIC  X(08).
      *
      *���������ɡʣ���Ƚ��ǯ��顼���������Ը������顼��
           03  LNK-HOSPCHG-RC                        PIC S9(9) BINARY.
      *
