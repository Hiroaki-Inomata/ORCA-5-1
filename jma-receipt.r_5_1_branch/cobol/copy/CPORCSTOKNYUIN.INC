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
      *    ���������������ɼ����ѥ�᥿����COPY (CPORCSTOKNYUIN.INC)
      *                                ���ѥץ������ (ORCSTOKNYUIN)
      *    �������
      *      ������������ʬ
      *      ����������
      *      ����ǯ��
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSTOKNYUIN-AREA.
      *
      *������������ʬ�ʣ������ŵ��ء�������������¼���
           03  LNK-TOKNYUIN-KBN                       PIC  X(01).
      *����������
           03  LNK-TOKNYUIN-NYUIN                     PIC  X(02).
      *����ǯ��
           03  LNK-TOKNYUIN-SANTEIYM                  PIC  X(06).
      *�����ֹ�
           03  LNK-TOKNYUIN-BTUNUM                    PIC  X(02).
      *���������������׷��ʬ�ʶ��򡧳������롿�����������ʤ���
           03  LNK-TOKNYUIN-SANTEIYOUKENKBN           PIC  X(01).
      *�ֵѹ���
           03  LNK-TOKNYUIN-HENKYAKU-AREA.
      *����������������
               05  LNK-TOKNYUIN-SRYCD                 PIC  X(09).
      *���������������׷ﳰ������
               05  LNK-TOKNYUIN-GAI-SRYCD             PIC  X(09).
      *���������������׷ﳰ���������������������ɤ�������������û�
      *���������������������������ˤ�껻�ꤹ�����Ƚ���ʬ
      *�ʶ����̾�βû����꡿������������������������������ǲû������
               05  LNK-TOKNYUIN-GAI-NYUKSNKBN         PIC  X(01).
      *���������ɡʣ��������������ֵѤ��ꡡ���������������ֵѤʤ���
           03  LNK-TOKNYUIN-RC                     PIC S9(9) BINARY.
      *