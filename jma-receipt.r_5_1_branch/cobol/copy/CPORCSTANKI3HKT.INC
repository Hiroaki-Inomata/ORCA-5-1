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
      *    û���ںߣ��������å��ѥ�᥿��COPY (CPORCSTANKI3HKT.INC)
      *                                ���ѥץ������ (ORCSTANKI3HKT)
      *    �������
      *      ���ŵ����ֹ�
      *      ���ԣɣ�
      *      ����ǯ����
      *      ���Ź԰٥�����
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-¿��Ǽ  17/04/XX  ���θ��������û�����б�
      ************************************************************
       01  ORCSTANKI3HKT-AREA.
      *
      *���ŵ����ֹ�
           03  LNK-TANKI3HKT-HOSPNUM           PIC  9(2).
      *���ԣɣ�
           03  LNK-TANKI3HKT-PTID              PIC  9(10).
      *����ǯ����
           03  LNK-TANKI3HKT-SANTEIYMD         PIC  X(08).
      *���Ź԰٥�����
           03  LNK-TANKI3HKT-SRYCD             PIC  X(09).
      *H29.4
      *����û���ںߣ����ˤ�껻���Բ��оݤȤ���
           03  LNK-TANKI3HKT-HKTKBN            PIC  X(01).
      *�ֵѹ���
           03  LNK-TANKI3HKT-HENKYAKU-AREA.
      *����ʬ�ʶ��򡧻���ġ�����û���ںߣ����ˤ�껻���Բġ�
             05  LNK-TANKI3HKT-KBN             PIC  X(01).
      *H29.4
      *����ʬ���ʣ���û���ںߣ����ˤ�껻��Ĥθ���Ƚ������
             05  LNK-TANKI3HKT-KBN2            PIC  X(01).
      *
      *���������ɡʣ������Ը������顼����������ǯ�������顼��
           03  LNK-TANKI3HKT-RC                PIC S9(9) BINARY.
      *