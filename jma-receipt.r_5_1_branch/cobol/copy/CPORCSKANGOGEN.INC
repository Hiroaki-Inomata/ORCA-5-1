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
      *    ��ִǸ���������������������ʿ������ǯ�ٲ�����
      *                             �ѥ�᥿��COPY (CPORCSKANGOGEN.INC)
      *                             ���ѥץ������ (ORORCSKANGOGEN)
      *    �������
      *      ���ŵ����ֹ�
      *      ���ԣɣ�
      *      ����ǯ��
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSKANGOGEN-AREA.
      *
      *���ŵ����ֹ�
           03  LNK-KANGOGEN-HOSPNUM           PIC 9(2).
      *���ԣɣ�
           03  LNK-KANGOGEN-PTID              PIC  9(10).
      *����ǯ��
           03  LNK-KANGOGEN-SANTEIYM          PIC  X(06).
      *�ֵѹ���
           03  LNK-KANGOGEN-HENKYAKU-AREA.
             05  LNK-KANGOGEN-TBL.
      *        ��ִǸ���������������������
               07  LNK-KANGOGEN-OCC  OCCURS  31.
                   09  LNK-KANGOGEN-SRYCD         PIC  X(09).
                   09  LNK-KANGOGEN-TEN           PIC  9(05).
      *
      *���������ɡʣ����±��ʳ�����������ǯ��顼��
      *            �������Ը������顼������������פʤ�
      *            ���������˸��۳�����������̵����
           03  LNK-KANGOGEN-RC                PIC S9(9) BINARY.
      *
