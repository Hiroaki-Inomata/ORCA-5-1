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
      *    ���Ŵѻ������������ɼ����ѥ�᥿����COPY (CPORCSIRYOKNS.INC)
      *                                ���ѥץ������ (ORCSIRYOKNS)
      *    �������
      *      ���ŵ��ؼ����ֹ�
      *      ������������ʬ
      *      ����������
      *      �����ֹ�
      *      ����ǯ��
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSIRYOKNS-AREA.
      *
      *���ŵ��ؼ����ֹ�
           03  LNK-IRYOKNS-HOSPNUM                    PIC  9(02).
      *������������ʬ�ʣ������ŵ��ء�������������¼����������Ŵѻ���
           03  LNK-IRYOKNS-KBN                        PIC  X(01).
      *����������
           03  LNK-IRYOKNS-NYUIN                      PIC  X(02).
      *�����ֹ�
           03  LNK-IRYOKNS-BTUNUM                     PIC  X(02).
      *����ǯ��
           03  LNK-IRYOKNS-SANTEIYM                   PIC  X(06).
      *�ֵѹ���
           03  LNK-IRYOKNS-HENKYAKU-AREA.
      *���Ŵѻ�������������
               05  LNK-IRYOKNS-SRYCD                  PIC  X(09)
                                                      OCCURS  10.
      *���������ɡʣ������Ŵѻ��������ֵѤ��ꡡ�������Ŵѻ��������ֵѤʤ���
           03  LNK-IRYOKNS-RC                         PIC S9(9) BINARY.
      *
