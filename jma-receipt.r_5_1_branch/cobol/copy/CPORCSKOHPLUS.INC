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
      *    �����ղþ�������ѥѥ�᥿    COPY (CPORCSKOHPLUS.INC)
      *                                  ���ѥץ������ (ORCSKOHPLUS)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ֹ�
      *      ���ԣɣ�
      *      �ݸ��ȹ礻
      *      ����ǯ����
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSKOHPLUS-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-SKOHPLUS-HOSPNUM                 PIC 9(2).
      *���ֹ�
           03  LNK-SKOHPLUS-PREFNUM                 PIC  X(02).
      *���ԣɣ�
           03  LNK-SKOHPLUS-PTID                    PIC  9(10).
      *�ݸ��ȹ礻
           03  LNK-SKOHPLUS-HKNCOMBI                PIC  9(04).
      *����ǯ����
           03  LNK-SKOHPLUS-SRYYMD                  PIC  X(08).
      *�ֵѹ���
           03  LNK-SKOHPLUS-HENKYAKU-AREA.
      *��ô��ʬ
               05  LNK-SKOHPLUS-FTNKBN              PIC  X(01).
      *�����ݿ������ܶ�ʬ
               05  LNK-SKOHPLUS-NYUSKJRYYKBN        PIC  X(01).
      *�쥻�ץ������ʬ
               05  LNK-SKOHPLUS-RECESKYKBN          PIC  X(01).
      *
      *����������
           03  LNK-SKOHPLUS-RC                      PIC S9(9) BINARY.
      *
