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
      *    ������ô�����֤��оݼ����ѥ�᥿��COPY (CPORCSSKJKEEP.INC)
      *                            ���ѥץ������ (ORORCSSKJKEEP)
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
       01  ORCSSKJKEEP-AREA.
      *
      *���ŵ����ֹ�
           03  LNK-SKJKEEP-HOSPNUM             PIC 9(2).
      *���ԣɣ�
           03  LNK-SKJKEEP-PTID                PIC  9(10).
      *����ǯ��
           03  LNK-SKJKEEP-SANTEIYM            PIC  X(06).
      *�ֵѹ���
           03  LNK-SKJKEEP-HENKYAKU-AREA.
      *������ô�����֤���ʬ
             05  LNK-SKJKEEP-TBL.
      *  �ʶ����оݳ������������������֤������������¾������֤���
               07  LNK-SKJKEEP-KBN             PIC  X(01)  OCCURS  31.
      *���¥����ƥ�ͽ�����϶�ʬ
      *  �ʶ����оݳ����������¿����֤����������������ˤ��ž���
               07  LNK-SKJKEEP-NANBYOKBN       PIC  X(01)  OCCURS  31.
      *�����¾�����ǯ����
             05  LNK-SKJKEEP-SEI-NYUINYMD      PIC  X(08).
      *�����¾�����ǯ���������ȼ����϶�ʬ
      *  �ʶ���̤���ϡ����������ϡ�
             05  LNK-SKJKEEP-SEI-COMMENT-KBN   PIC  X(01).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ����������פʤ������������������������ο����֤��оݳ�
      *            ���������¾������֤����ϣ�����Ķ���
           03  LNK-SKJKEEP-RC                  PIC S9(9) BINARY.
      *
